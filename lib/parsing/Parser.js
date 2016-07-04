define([
    'Atem-Property-Language/errors'
  , './helpers'
  , './_ValueToken'
  , './OperatorToken'
  , './BracketToken'
  , './StringToken'
  , './SelectorToken'
  , './NumberToken'
  , './NameToken'
], function(
    errors
  , helpers
  , _ValueToken
  , OperatorToken
  , BracketToken
  , StringToken
  , SelectorToken
  , NumberToken
  , NameToken
) {
    "use strict";

    var PropertyLanguageError = errors.PropertyLanguage
      , KeyError = errors.Key
      , isInstance = helpers.isInstance
      ;

    /**
     * Constructor for a CPS formulae Parser. This takes instances of
     * OperatorToken as Input.
     */
    function Parser(/* operators */) {
        this._operators = this._createOperatorsDict(
                Array.prototype.slice.call(arguments));


        this._operatorsByLength = this._createLengthLookup(
                                                        this._operators);

        this._bracketOperators = {};
        this._negateOperator = undefined;
        this._StackConstructor = undefined;
    }

    var _p = Parser.prototype
        /**
         * Test if a string starts like a number. This detects also
         * negative numbers.
         * R_number.exec(string) !== null
         */
      , R_number = /^(\-?((\d*\.\d+)|(\d+(\.)?))([eE][+\-]?\d+)?)/
        //  Test if a string starts like a name
      , R_name = /^[0-9A-Za-z_]+/
      ;

    _p._createOperatorsDict = function(operators) {
        var i = 0
          , result = {}
          ;
        for(;i<operators.length;i++) {
            if(operators[i].literal in result)
                throw new PropertyLanguageError('An operator with the literal "'
                                + operators[i].literal +'" is defined at '
                                + 'least twice, but it must be unique!');
            if(R_number.exec(operators[i].literal) !== null)
                throw new PropertyLanguageError('The operator with the literal "'
                                + operators[i].literal +'" starts like a '
                                + 'number literal. This is forbidden.');
            result[operators[i].literal] = operators[i];
        }
        return result;
    };

    /**
     * returns an object with the keys "splitting" and "notSplitting"
     * both keys contain an array of objects.
     * The objects are key value pairs of operatorLiteral: operator
     * The operators in one object have all the same length.
     * The arrays are sorted in the way that the objects with the longest
     * operatorLiterals appear first;
     *
     *  {
     *      splitting: [ operator literals by length lookup ]
     *      notSplitting: [ operator literals by length lookup ]
     *  }
     *
     * the array for the "operator literals by length lookup":
     * // ordered by operator.literal.length, longest first
     * [
     *      // all literalName in these objects have the same length
     *      {literalName_A: operator, literalName_B: operator}
     *    , {literalN_A: operator, literalN_B: operator}
     *      ...
     * ]
     */
    _p._createLengthLookup = function(operators) {
        var _get = function(k) {return this[k];}
          , k
          , _temp
          , temp = {
                splitting: {}
              , notSplitting: {}
            }
          , result = {}
        ;

        // put everything in the right temp container
        for(k in operators) {
            _temp = operators[k].splitting
                ? temp.splitting
                : temp.notSplitting
                ;
            if(_temp[k.length] === undefined)
                _temp[k.length] = {};
            _temp[k.length][k] = operators[k];
        }

        // sort and maintain order by returning arrays of operator dicts
        for(k in temp)
            result[k] = Object.keys(temp[k])
                              // sort by "k.length" keys
                              .sort()
                              // longest first
                              .reverse()
                              // return the operator dicts
                              .map(_get, temp[k]);
        return result;
    };

    _p.setStackConstructor = function(ctor){
        this._StackConstructor = ctor;
    };

    _p.setBracketOperator = function(bracketLiteral, operatorLiteral) {
        if(!(operatorLiteral in this._operators))
            throw new KeyError('No operator found for literal: '
                                                        + operatorLiteral);

        this._bracketOperators[bracketLiteral] = operatorLiteral;
    };

    _p.hasBracketOperator = function(bracketLiteral) {
        return bracketLiteral in this._bracketOperators;
    };

    _p.getBracketOperator = function(bracketLiteral) {
        return this._operators[this._bracketOperators[bracketLiteral]];
    };

    _p.setNegateOperator = function(negateLiteral, operatorLiteral) {
        if(!(operatorLiteral in this._operators))
            throw new KeyError('No operator found for literal: '
                                                        + operatorLiteral);
        this._negateOperator = [negateLiteral, operatorLiteral];
    };

    /**
     * test if string starts with the operator.literal of one of the
     * operators in the operators list.
     *
     * The operators list has the following structure (to avoid a linear search)
     *
     * // ordered by operator.literal.length
     * [
     *      // all literalName keys in here have the same length
     *      {literalName: operator}
     *      ...
     *  }
     * ]
     *
     *
     */
    function _testOperators(operators, string, index) {
        var i=0, k, search;
        for(;i<operators.length;i++) {
            // get the first key
            k = null;
            for(k in operators[i])
                break;
            if(k === null)
                continue;
            // cut out the right length from string
            search = string.substr(index, k.length);
            if(operators[i].hasOwnProperty(search))
                // search is a key in operators
                return operators[i][search];
        }
        return false;
    }

    /**
     * Test for all NOT splitting operators, longest first.
     */
    _p._testNotSplittingOperators = function(string, index) {
        return _testOperators(this._operatorsByLength.notSplitting
                                                        , string, index);
    };

    /**
     * Test for all splitting operators, longest first.
     */
    _p._testSplittingOperators = function(string, index) {
        return _testOperators(this._operatorsByLength.splitting
                                                        , string, index);
    };


    /**
     * Tokenize into the following tokens:
     *
     * number literals: anything that ufojs/main.isFloatString accepts
     *      1 .3 -1.2 1.2e3  3E3 0.123456E-3 etc..
     *
     * selector literals: anything between S" AND "  S"master#bold > glyph:i(3)"
     *      we keep the quotes, because some characters that can appear
     *      in selectors could cause problems with our CSS/CPS parser in
     *      the context of a parameter value
     * string literals: anything between " AND "
     * parenthesis: ( and )
     * Square brackets [ and ]  <= will essentially behave like a stack ()
     *              but the resulting value will be used as key to get a
     *              value from the previous value in the stack
     *              So, this resolves to a similar thing like the colon
     *              operator. but the colon operator will use the literal
     *              of a NameValue AND thus require a NameValue
     *              we may get rid of the colon operator but then find us
     *              typing a lot of [" AND "] combinations ...
     *
     * names/identifier: essentially every token that is not something else ...
     *            maybe it is wise to identify a set of legal characters,
     *            like 0-9A-Za-z_ this could save space for new additions
     *            also, this eases parsing
     *            name can't begin with numbers, because of the splitting
     *            behavior of numbers at the moment.
     *
     * operators/symbols: identifier that are keys in this._operators
     *
     * special is the "negate" operator, which will be inserted on some
     * occasions where - appears. But this is not done in this context
     * the parser will do so.
     *
     * splitting is done by:
     *  ' ' space
     *  $" " selector literal
     *  " " string literal
     *  \n newline
     *  \r carriage return
     *  \t tab
     *  all operators where operator.splits === true
     *          if it doesn't split it can be part of a 'name'
     *
     * special in terms of splitting is the . operator
     *      it splits, but only if it is not part of a number literal!
     *
     *
     * in the end, we expect a list of:
     *
     * - number values from number literals
     * - selector values from selector literals
     * - string values from string literals
     * - brackets: one of these four at a time ( ) [ ]
     * - operators
     * - names
     *
     *
     * selectorEngine is optional, it will cause a selector to be compiled
     * immediately, contrary to beeing compiled when first used.
     */
    _p.tokenize = function(string, selectorEngine) {
        var i=0, tokenEnd
          , tokens = []
          , reResult
          , splitExpected
          , foundOperator
          ;
        while(i<string.length) {
            if(' \n\r\t'.indexOf(string[i]) !== -1) {
                // stuff that splits but is not reported (whitespace)
                i++;
                splitExpected = false; // a splitting token was found
                continue;
            }

            // brackets are splitting
            if('()[]'.indexOf(string[i]) !== -1) {
                tokens.push(new BracketToken(string[i]));
                i++;
                splitExpected = false; // a splitting token was found
                continue;
            }

            // string literals are splitting
            if(string[i] === '"') {
                tokenEnd = string.indexOf('"', i+1);
                if(tokenEnd === -1)
                    throw new PropertyLanguageError('A closing double quote is '
                        +' missing for an opening string literal: "');
                tokens.push(new StringToken(string.substring(i+1, tokenEnd)));
                i = tokenEnd+1;
                splitExpected = false; // a splitting token was found
                continue;
            }

            // selector literals are splitting
            if(string[i] === 'S' && string[i+1] === '"') {
                tokenEnd = string.indexOf('"', i+2);
                if(tokenEnd === -1)
                    throw new PropertyLanguageError('A closing double quote is '
                        +' missing for an opening selector literal S" ...in: '
                        + string.substr(i));
                tokens.push(new SelectorToken(string.substring(i+2, tokenEnd), selectorEngine));
                i = tokenEnd+1;
                splitExpected = false; // a splitting token was found
                continue;
            }

            // number literals are splitting, thus we can parse negative
            // numbers. (maybe they must not be splitting, but they
            // must be parsed before the splitting operators?)
            // FIXME: I'm not sure if I like this rather hackish workaround.
            // Instead of making numbers splitting, we could maybe have
            // a more robust way to detect the "negate" operator, unfortunately
            // this: "Vector 12 -8" makes it really hard to do so. It can
            // read as "Vector 12 subtract 8" or "Vector 12 negate 8" without
            // having splitting numbers the former applies but the latter
            // is meant.
            // Also, names can't begin with numbers anymore, because of this
            // behavior, however, this quite common in other programming
            // languages as well.
            // The biggest downside of this behavior is that:
            // "1-2" parses as `1|-2` and "1 - 2" parses as
            // `1|subtract|2` which will become confusing at some point.
            string = string.substring(i);
            i = 0;
            if((reResult = R_number.exec(string)) !== null) {
                tokens.push(new NumberToken(reResult[0]));
                i = reResult[0].length;
                splitExpected = false; // a splitting token was found
                continue;
            }

            // test for all splitting operators, length first
            if(!!(foundOperator = this._testSplittingOperators(string, i))) {
                tokens.push(foundOperator);
                i += foundOperator.literal.length;
                splitExpected = false; // a splitting token was found
                continue;
            }

            // END OF SPLITTING TOKENS

            // The last found token was expecting as next token a splitting
            // token, because it was not splitting by itself.
            // A splitting token was not found.
            if(splitExpected === true)
                throw new PropertyLanguageError('A splitting token was expected '
                                + 'after: '+ tokens[tokens.length-1] + ' '
                                + 'but it was not found in: '
                                + string.substr(i));

            // From here we expect to find a not splitting token
            // the token after that must be splitting
            // if we don't find anything a PropertyLanguageError is thrown
            splitExpected = true;

            // prepare for RegEx.exec searches
            // the string must be truncated to the current index
            // because RegEx.exec has no offset parameter like indexOf
            string = string.substr(i);
            i=0;

            // name literals are not splitting
            if((reResult = R_name.exec(string)) !== null) {
                if(reResult[0] === 'Infinity')
                    tokens.push(new NumberToken(reResult[0]));
                else if(this._operators[reResult[0]] && !this._operators[reResult[0]].splitting)
                    // could also be a not splitting operator
                    tokens.push(this._operators[reResult[0]]);
                else
                    tokens.push(new NameToken(reResult[0]));
                i += reResult[0].length;
                continue;
            }

            // test for all NOT splitting operators, length first
            if(!!(foundOperator = this._testNotSplittingOperators(string, i))) {
                tokens.push(foundOperator);
                i += foundOperator.literal.length;
                continue;
            }

            // not recognized as token!
            throw new PropertyLanguageError('Can\'t find the next token in the '
                                    + 'string: ' + string);
        }
        return tokens;
    };

    /**
     * needed only for debugging
     */
    function tokensToString(tokens, asArray) {
        var i, result = [], sub;
        for(i=0;i<tokens.length;i++) {
            if(tokens[i] instanceof Array){
                sub = tokensToString(tokens[i], asArray);
                result.push( asArray ? sub  : '('+  sub +')' );
            }
            else
                result.push(tokens[i] ? tokens[i].literal : tokens[i]);
        }
        if(asArray)
            return result;
        return result.join(' ');
    }
    Parser.tokensToString = tokensToString;

    /**
     * Returns a list of Operator indexes in tokens sorted in the
     * order they have to be resolved.
     *
     * This uses the original order and the precedence of the operators.
     *
     * FIXME: this is probably be the right place to apply the associativity
     *        as well, which we don't do yet.
     *        Maybe a reordering in the operators[precedence] list using
     *        associativity in some way would do the trick? This would be
     *        very simple and stupid.
     *        One problem I see with this approach is that not all operators
     *        in a operators[precedence] list affect each other, so a total
     *        reordering in that list might be too much!
     */
    function _getOperatorIndexes(tokens) {
        var operators = Object.create(null)
          , precedences = []
          , bucket
          , token, i
          ;
        for(i=0;i<tokens.length;i++) {
            token = tokens[i];
            if(!(token instanceof OperatorToken))
                continue;
            bucket = operators[token.precedence];
            if(!bucket) {
                operators[token.precedence] = bucket = [];
                precedences.push(token.precedence);
            }
            bucket.push(i);
        }

        function biggestFirst (a, b) { return b-a; }
        function inline(previous, precedence) {
            return previous.concat(operators[precedence]);
        }
        return precedences.sort(biggestFirst)
                          .reduce(inline, [])
                          ;
    }

    function Operation(operatorToken, args) {
        Object.defineProperties(this, {
            operator: {
                value: operatorToken
              , enumerable: true
            }
          , arguments: {
                value: args
              , enumerable: true
            }
        });
    }
    function Grouping(groupingToken, node) {
        Object.defineProperties(this, {
            groupingToken: {
                value: groupingToken
              , enumerable: true
            }
          , node: {
                value: node
              , enumerable: true
            }
        });
    }

    function _toASTNode(tokens) {
        var i, j, k, l, token
          , preConsumes, postConsumes, startPre, startPost, endPost
          , argument, operator, args
          , operatorIndexes = _getOperatorIndexes(tokens)
          ;

        // the stack has no brackets anymore. Instead, Arrays which must be
        // treated like _ValueTokens.
        for(j=0,l=operatorIndexes.length;j<l;j++) {
            i = operatorIndexes[j];
            token = tokens[i];
            // Array and Value don't change the stack, only OperatorToken
            // does. Thus Array and Value stay where they are until
            // they are consumed by an Operator.

            // token is an Operator

            // If preConsumes is Infinity, the operator consumes
            // anything that is on the stack before its position.
            // This is useful for some kind of list creation.
            preConsumes = token.preConsumes === Infinity
                ? i
                : token.preConsumes
                ;

            startPre = i - preConsumes;
            if(startPre < 0) {
                throw new PropertyLanguageError('Stack underflow at a "'+token+'" '
                                + 'operator, which pre-consumes more items '
                                + 'than there are on the stack');
            }
            // skip the operator itself
            startPost = i+1;

            // If postConsumes is Infinity, the operator consumes
            // anything that is on the stack after its position.
            // This is useful for some kind of list creation.
            postConsumes = token.postConsumes === Infinity
                ? tokens.length - startPost
                : token.postConsumes
                ;

            endPost = startPost + postConsumes;
            if(endPost > tokens.length) {
                throw new PropertyLanguageError('Stack underflow at a "'+token+'" '
                                + ' which post-consumes more items '
                                + 'than there are on the stack');
            }
            args = tokens.slice(startPre, i);
            Array.prototype.push.apply(args, tokens.slice(startPost, endPost));

            // check if everything looks alright
            for(k=0; k<args.length; k++) {
                argument = args[k];

                if(!isInstance(argument, [_ValueToken, Grouping, Operation]))
                    throw new PropertyLanguageError('Malformed stack at a "'
                        + token.literal+'" operator, which consumes '
                        + (argument instanceof OperatorToken
                            ? 'another operator: "' + argument.literal + '"'
                            : 'something that is not a ValueToken: "'
                                + argument + '" typeof: '
                                + typeof argument
                                + ' '+argument.constructor.name));
            }
            // add the operator

            operator = token.preConsumes === Infinity
                                        || token.postConsumes === Infinity
                    ? token.fixedConsumptionFactory(preConsumes, postConsumes)
                    : token
                    ;

            // change tokens in place, Operation will be handled as a single
            // value in later iterations
            tokens.splice(startPre, args.length + 1, new Operation(operator, args));

            // adjust for the change caused by splice
            if(args.length) {
                // for all remaining operatorIndexes === beginning from j
                for(k=j;k<l;k++)
                    // if pointing after startPre
                    if(operatorIndexes[k] > startPre)
                        // the stack changed by the amount of adjust
                        // so the index must change
                        operatorIndexes[k] -= args.length;
            }
        }

        // TODO: only for devloping, remove again for speed.
        // Also note that stacks that resolve to more than one item may
        // become a thing we want to do.
        // This is also interesting, because it means that our "subroutines"
        // really are mere groupings.
        if(tokens.length !== 1) {
            console.log('tokens.length !== 1', tokens.length, tokensToString(tokens));
            throw new errors.Assertion('Tokens don\'t resolve to a single item.');
        }

        return tokens[0];
    }

    /**
     * Take the tokens where the calculations are in an infix notation and
     * return postfix or Reverse Polish notation:
     * This means we go from 2 + 3 to 2 3 +. The operator follows all
     * of its operand. This is easy to calculate at the end, and we get
     * rid of the Parenthesis. See ./Stack.execute for execution of the
     * stack.
     *
     * This works as far as my tests went, but it could be more efficiently
     * implemented (using the "Dijkstra shunting yard algorithm"?)
     *
     * The algorithm uses one recursive call to eliminate parentheses
     * and multiple passes to solve all operators in order of precedence.
     */
    _p.infixToAST = function (tokens) {
        var i, l, token
          , node, finished
          // used for destructuring
          , currentStack = [null, []]
          , currentBracket, current, result
          , openStacks = []
          ;

        currentBracket = currentStack[0];
        current = result = currentStack[1];
        for(i=0,l=tokens.length;i<l;i++) {
            token = tokens[i];
            // replace - with negate when it looks like this was the intention
            if(this._negateOperator
                        && token instanceof OperatorToken
                        // usually we use - to negate something
                        && token.literal === this._negateOperator[0]
                        // if the first operator is a subtract operator
                        // or if the operator before the subtract operator
                        // is any operator, then this is a negate operator
                        && ( i===0
                            || tokens[i-1] instanceof OperatorToken
                            || tokens[i-1] instanceof BracketToken) ) {
                // We do this here, so that we can reproduce the input string
                // again, using _p.astToInfix. However, if we're ever going
                // to serialize the AST, I see this as a problem for a KISS
                // representation. We'll see.
                // Note, if the input string contained a literal "negate"
                // we want it to be preserved. That's why we mark each
                // instance where a "-" is replaced by a "negate".
                token = Object.create(this._operators[this._negateOperator[1]]);
                Object.defineProperty(token, 'literal', {
                    value: tokens[i].literal
                });
            }

            // resolve brackets
            if(!(token instanceof BracketToken))
                current.push(token);
            // it is a bracket
            else if(token.opening) {
                openStacks.push(currentStack);
                currentBracket = token;
                current = [];
                currentStack = [currentBracket, current];

            }
            else { // tokens[i].closing === true
                if(!openStacks.length
                        || !currentBracket.matches(token.literal))
                    throw new PropertyLanguageError('Mismatched brackets: "'
                            + 'missing opening bracket for "'
                            + token.literal + '".' + (currentBracket && currentBracket.literal));

                // the closing bracket matches an opening bracket

                // finalize the current stack;


                // the returned ASTNode is either an Operation, a _Valuetoken
                // or another Grouping
                node = _toASTNode(current);
                finished = new Grouping(currentBracket, node);

                currentStack = openStacks.pop();
                currentBracket = currentStack[0];
                current = currentStack[1];
                if(this.hasBracketOperator(token.counterpart))
                    // If an operator is registered for this bracket,
                    // we insert it, so that it consumes the value
                    // of the bracket.
                    // This is actually used if this is a [] context
                    // We insert a getter operator then, that will
                    // consume the content of the [] stack and uses
                    // its value as a key to read from in the previous
                    // value like: myValue["myKey"]
                    // becomes: myValye __get__ "myKey"
                    // becomes: myValye "myKey" __get__
                    current.push(this.getBracketOperator(token.counterpart));

                current.push(finished);
            }
        }
        if(openStacks.length)
            throw new PropertyLanguageError(openStacks.length + ' '
                    + (openStacks.length > 1 ? 'bracket is'
                                                : 'brackets are' ) + ' '
                    + 'missing for the opened: '
                    + openStacks.map(function(item){ return item.literal; })
                               .join(', '));

        // the root Grouping has no currentBracket
        return new Grouping(null, _toASTNode(result));
    };

    function astToPostfix(node) {
        var tokens = [node], i=0, item, replacement;
        while(i<tokens.length) {
            item = tokens[i];
            if(item instanceof Grouping) {
                // replace the item in tokens with the value of the Grouping
                tokens.splice(i, 1, item.node);
                // don't increment, visit this index in the next round
                continue;
            }
            else if(item instanceof Operation) {
                // replace the item in tokens with the value of the Operation
                // splice arguments i and 1
                replacement = [i, 1];
                Array.prototype.push.apply(replacement, item.arguments);
                replacement.push(item.operator);
                // change in place
                Array.prototype.splice.apply(tokens, replacement);
                // don't increment, visit this index in the next round
                continue;
            }
            i++;
        }
        return tokens;
    }

    /**
     * We can't yet properly print code from tokens, but we should be able
     * to recreate the infix tokens stack exactly
     */
    _p.astToInfix = function (node) {
        var tokens = [node], i=0, item, replacement, bracketOperator;
        while(i<tokens.length) {
            item = tokens[i];
            if(item instanceof Grouping) {
                replacement = [i, 1];
                if(item.groupingToken !== null) {
                    if(this.hasBracketOperator(item.groupingToken.literal)) {
                        // it should be already on the stack.
                        bracketOperator = this.getBracketOperator(item.groupingToken.literal);
                        errors.assert(tokens[i-1] === bracketOperator, tokens[i-1]);
                        i = i-1;
                        replacement = [i, 2];
                    }
                    replacement.push(item.groupingToken);
                }
                replacement.push(item.node);
                if(item.groupingToken !== null)
                    replacement.push(new BracketToken(item.groupingToken.counterpart));
                Array.prototype.splice.apply(tokens, replacement);
                // don't increment, visit this index in the next round
                continue;
            }
            else if(item instanceof Operation) {
                replacement = [i, 1];
                Array.prototype.push.apply(replacement
                    , item.arguments.slice(0, item.operator.preConsumes));
                // We want the vanilla thing, not the instance possibly created
                // by OperatorToken.prototype.fixedConsumptionFactory.
                // That way we don't have surprises when round tripping.
                replacement.push(this._operators[item.operator.literal]);
                Array.prototype.push.apply(replacement
                    , item.arguments.slice(-item.operator.postConsumes));

                Array.prototype.splice.apply(tokens, replacement);
                // don't increment, visit this index in the next round
                continue;
            }
            i++;
        }
        return tokens;
    };

    /**
     * selectorEngine is optional, it will cause a selector to be compiled
     * immediately, contrary to beeing compiled when first used.
     */
    _p.parse = function(string, selectorEngine) {
        if(!this._StackConstructor)
            throw new PropertyLanguageError('StackConstructor is missing. Run engine.setStackConstructor before running engine.parse.');

        // We want to be able to change this and put it out as a PL string again
        // this is not what we are doing right now!
        var tokens = this.tokenize(string, selectorEngine)
          , ast = this.infixToAST(tokens)
          , postfix = astToPostfix(ast)
          // , infix = this.astToInfix(ast)
          ;


        if(!postfix.length)
            throw new PropertyLanguageError('The input string did not produce any instructions.');
        return new this._StackConstructor(postfix);
    };

    return Parser;
});
