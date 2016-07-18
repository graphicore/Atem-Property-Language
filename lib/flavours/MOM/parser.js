define([
    'Atem-Property-Language/errors'
  , 'Atem-Property-Language/parsing/Parser'
  , 'Atem-Property-Language/parsing/OperatorToken'
  , 'Atem-Property-Language/parsing/NameToken'
  , 'Atem-Property-Language/parsing/_Token'
  , 'Atem-CPS/CPS/elements/SelectorList'
  , 'Atem-CPS/CPS/parsing/parseSelectorList'
  , 'Atem-CPS/OMA/_Node'
  // The Atem-Math-Tools dependency is added to Atem-Property-Language just
  // for this MOM-flavour. Make an Atem-MOM-Property-Language stand-alone module?
  , 'Atem-Math-Tools/transform'
  , 'Atem-Math-Tools/Vector'
  , 'Atem-Math-Tools/hobby'
  , 'Atem-Math-Tools/utils'
  , 'Atem-Property-Language/UI'
], function(
    errors
  , Parser
  , Operator
  , NameToken
  , _Token
  , SelectorList
  , parseSelectorList
  , _OMANode
  , transform
  , Vector
  , hobby
  , mathUtils
  , UI
) {
    "use strict";

    var PropertyLanguageError = errors.PropertyLanguage
      , Transformation = transform.Transform
      , engine
      , parseSelectorListFromString = parseSelectorList.fromString
      ;

    /**
     * This defines the operators that are usable in CPS-formulae, thus
     * the better part of the language definition can be found in here.
     * However, some rather specific behaviors are still buried in the
     * Parser implementation
     *
     * see the reference of new Operator for a description of its arguments.
     *
     * usage: engine.parse(CPSParameterValueString)
     */
    engine = new Parser(
        /**
         * returns an Array of everything that is on the current stack
         *
         * May become useful in the future, but for now it's more on the
         * experimentation side. Getters should already work on arrays, so
         * it may be a way to store values in an @dictionary parameter and
         * index numbers.
         */
        new Operator('List', false, -Infinity, 0, Infinity, function(/*args, ...*/) {
            return Array.prototype.slice.call(arguments);
        })
        /**
         * create a generic user interface marker.
         * the first argument will probably be the value to be represented
         * by a user interface. The rest of the arguments may be used for
         * options etc.
         * The type of the first argument will be used to determine the
         * interface for the rest of the arguments.
         */
      , new Operator('UI', false, -Infinity, 0, Infinity, (function(){
            // Don't be distracted by the closure!
            // It's used it to set `uiOperator.flags` in place and thus
            // the definition of this operator needs not be move to somewhere
            // else. Also, the namespace is a bit less polluted.
            function uiOperator (getAPI, astOperation /*, args, ...*/) {
                var i, l
                  , args = []
                  , _pr = getAPI.getCurrentPropertyAndRule()
                  , property = _pr[0]
                  , rule = _pr[1]
                  ;
                for(i=2,l=arguments.length;i<l;i++)
                    args.push(arguments[i]);
                return new UI(rule, property, astOperation, args);
            }
            // NOTE: the optional flag will cause astOperation to be
            // injected here. That's a bit hackish though! It works
            // with using the `keepASTOperations` flag of `Parser.astToPostfix`
            // and with `ASTOperation` mimicking the API of `OperatorToken`
            // BUT injecting itself as last argument in the call to
            // this `Operator`
            uiOperator.flags = '*unboxed+getAPI,optional*';
            return uiOperator;
        })()
        )
        /**
         * Returns a generic Value, could be virtually anything
         *
         * used in a context like this
         * item['key']
         * which is translated to
         * item __get__ 'key'
         *
         * which should translate roughly to the javascript:
         * item['key'] or item.get('key'), depending on the nature
         * of item and the details of the implementation
         */
      , new Operator('__get__', false, Infinity, 1, 1, [
            ['*getAPI*', NameToken, 'string', function(getAPI, name, key) {
                var item = getAPI.get(name.getValue());
                return getAPI.genericGetter(item, key);
            }]
            // FIXME: I think a signature of
            // '*unboxed+getAPI*' ,'*anything*', '*anything*'
            // would do the same trick, also, the last operator implementation
            // here: '*unboxed+getAPI*','*anything*', ['number', 'string'] could be removed as well?
            // maybe, also the first... ????
          , ['*getAPI*', NameToken, NameToken, function(getAPI, name1, name2) {
                var item = getAPI.get(name1.getValue())
                  , key = getAPI.get(name2.getValue())
                  ;
                return getAPI.genericGetter(item, key);
            }]
            // value: this['parent'][S"point.top"]
          , ['*unboxed+getAPI*', _OMANode, SelectorList, function(getAPI, node, selector) {
                // internally does node.query(selector); but with dependency subscription
                var result = getAPI.query(node, selector);
                if(!result)
                    throw new PropertyLanguageError('Not found: an element for '
                                        + selector + ' '
                                        + 'in ' + node.particulars
                                    );
                return result;
            }]
          , ['*unboxed+getAPI*','*anything*', ['number', 'string'], function(getAPI, item, key) {
                return getAPI.genericGetter(item, key);
            }]
        ])
        /**
         * Returns a generic Value, could be virtually anything
         * similar to __get__
         *
         * used like this:
         * item:name
         *
         * name must be a name token, its value is used to get a propety
         * of item.
         * in javascript it does roughly the following:
         * var key = name.getValue()
         * return item[key]
         */
      , new Operator(':', true, Infinity, 1, 1, [
            ['*getAPI*', NameToken, NameToken, function(getAPI, name, key) {
                var item = getAPI.get(name.getValue());
                if(!item)
                    console.log('item:', item, 'from', '"'+name.getValue()+'"', 'key:', '"'+key.getValue()+'"');
                return getAPI.genericGetter(item, key.getValue());
            }]
          , ['*getAPI*', SelectorList, NameToken, function(getAPI, selector, key) {
                // SelectorList selects from global scope, aka multivers
                // var item = getAPI.get('this').multivers.query(selector);
                // FIXME: do instead
                // var item = getAPI.query(getAPI.get('this').multivers, selector); // internally node.query(selector); but with subscription

                // is some form of subscription needed for node.multivers???
                // maybe in the future, we will allow transports from one
                // multivers to another, then host.multivers can change
                var host = getAPI.get('this')
                  , node = getAPI.genericGetter(host, 'multivers')
                  , item = getAPI.query(node, selector)
                  ;


                if(!item)
                    throw new PropertyLanguageError('Not found: an element for '
                                                        + selector);
                return getAPI.genericGetter(item, key.getValue());
            }]
          , ['*getAPI*', '*anything*', NameToken, function(getAPI, item, key) {
                return getAPI.genericGetter(item, key.getValue());
            }]
        ])
        /**
         * When a value is negated using the minus sign, this operator is
         * inserted instead of the minus sign. It can also be used directly.
         *
         * The parser should detect cases where the minus sign is not a
         * subtraction, but a negation:
         *
         * -5 => negate 5
         * -(5 + name) => negate (5 + name)
         * 5 + -name => 5 + negate name
         * 5 + - name => 5 + negate name
         * name * - 5 => name * negate name
         *
         */
      , new Operator('negate', false, 60, 0, 1, [
            // 'number' as an argument is not needed nor happening
            // because something like -123 will be parsed as a negative
            // number directly. This is because "Vector 12 -8" would
            // otherwise be tokenized as "Vector 12 subtract 8", because
            // we have no other indication of splitting.
            // the operator is left in place, so this: --123 could be done
            // and would result in `negate -123`
            ['number', function(a){ return -a; }]
          , [Vector, function(a){ return a.negate();}]
          , [Transformation, function(transformation){ return transformation.inverse();}]
        ])
          /**
           * add
           */
      , new Operator('+', true, 10, 1, 1, [
            ['number' , 'number', function(a, b){ return a + b; }]
          , ['string' , 'string', function(a, b){ return a + b; }]
          , [Array , Array, function(a, b){ return a.concat(b); }]
          , [Vector, Vector, function(a, b){ return a['+'](b);}]
          , [Vector, 'number', function(a, b){ return a['+'](b);}]
        ])
        /**
         * subtract
         */
      , new Operator('-', true, 10, 1, 1, [
            ['number' , 'number', function(a, b){ return a - b; }]
          , [Vector, Vector, function(a, b){ return a['-'](b);}]
          , [Vector, 'number', function(a, b){ return a['-'](b);}]
        ])
        /**
         * multiply
         */
      , new Operator('*', true, 20, 1, 1, [
           ['number' , 'number', function(a, b){ return a * b; }]
         , [Vector, Vector, function(a, b){ return a['*'](b);}]
         , [Vector, 'number', function(a, b){ return a['*'](b);}]
         , [Transformation, Vector, function(tarnsformation, vector) {
                return Vector.fromArray(tarnsformation.transformPoint(vector));
           }]
         , [Transformation, Transformation, function(t1, t2) {return t1.transform(t2);}]
        ])
        /**
         * divide
         */
      , new Operator('/', true, 20, 1, 1, [
            ['number' , 'number', function(a, b){ return a / b; }]
          , [Vector, Vector, function(a, b){ return a['/'](b);}]
          , [Vector, 'number', function(a, b){ return a['/'](b);}]
        ])
        /**
         * pow
         *
         * FIXME: associativity must be set to 1 which means right-associative.
         */
      , new Operator('^', true, 30, 1, 1, [
            ['number' , 'number', function(a, b){ return Math.pow(a, b); }]
          , [Vector, Vector, function(a, b){ return a['**'](b);}]
          , [Vector, 'number', function(a, b){ return a['**'](b);}]
        ])
      , new Operator('min', true, 40, 0, 2, [
            ['number' , 'number', function(a, b){ return Math.min(a, b); }]
        ])
      , new Operator('max', true, 40, 0, 2, [
            ['number' , 'number', function(a, b){ return Math.max(a, b); }]
      ])
        /**
         * vector constructor operator
         * Creates a vector from Cartesian coordinates
         * Consumes two numbers returns a Vector
         */
      , new Operator('Vector', false, 40, 0, 2, [
            ['number' , 'number', function(a, b){ return new Vector(a, b); }]
        ])
        /**
         * vector constructor operator
         * Creates a vector from polar coordinates => magnitude angle in radians
         * Consumes two numbers returns a Vector
         */
      , new Operator('Polar', false, 40, 0, 2, [
            ['number' , 'number', function(a, b){ return Vector.fromPolar(a, b); }]
        ])
        /**
         * vector constructor operator
         * Creates a vector from two point coordinates, two directions
         * and one tension value;
         * The returned vector is the position of the outgoing control
         * of point0;
         *
         * Arguments: point0 outDir outTension inDir point1
         */
      , new Operator('tension2controlOut', false, 40, 0, 5, [
            [Vector , 'number', 'number', 'number', Vector, hobby.tension2controlOut]
        ])
        /**
         * vector constructor operator
         * Creates a vector from two point coordinates, two directions
         * and one tension value;
         * The returned vector is the position of the incoming control
         * of point1;
         *
         * Arguments: point0 outDir inTension inDir point1
         */
      , new Operator('tension2controlIn', false, 40, 0, 5, [
            [Vector , 'number', 'number', 'number', Vector, hobby.tension2controlIn]
        ])
        /**
         * Get the maginitude of the incoming control point.
         *
         * Arguments: point0 outDir inTension inDir point1
         */
      , new Operator('tension2magnitudeIn', false, 40, 0, 5, [
            [Vector , 'number', 'number', 'number', Vector, hobby.tension2magnitudeIn]
        ])
        /**
         * Get the maginitude of the outgoing control point.
         *
         * Arguments: point0 outDir outTension inDir point1
         */
      , new Operator('tension2magnitudeOut', false, 40, 0, 5, [
            [Vector , 'number', 'number', 'number', Vector, hobby.tension2magnitudeOut]
        ])
        /**
         * Get the tension of the outgoing control point.
         *
         * Arguments: point0 outDir outLength inDir point1
         */
      , new Operator('magnitude2tensionOut', false, 40, 0, 5, [
            [Vector , 'number', 'number', 'number', Vector, hobby.magnitude2tensionOut]
        ])
        /**
         * Get the tension of the incoming control point.
         *
         * Arguments: point0 outDir inLength inDir point1.
         */
      , new Operator('magnitude2tensionIn', false, 40, 0, 5, [
            [Vector , 'number', 'number', 'number', Vector, hobby.magnitude2tensionIn]
        ])

        /**
         * Convert a number from degree to radians
         * This has higher precedence than "polar" because it makes writing:
         * "polar 100 deg 45" possible.
         */
      , new Operator('deg', false, 50, 0, 1, [
            ['number', function(a) {
                return a * Math.PI/180;
            }]
        ])
        /**
         * Normalize `angle` given in radians between 0 and 2*PI
         */
      , new Operator('normalizeAngle', false, 50, 0, 1, [
            ['number', function(a) {
                return mathUtils.normalizeAngle(a);
            }]
        ])
        /**
         * Print information about the input value to console.log
         * and return the value again.
         * This doesn't change the result of the calculation.
         */
      , new Operator('_print', false, Infinity, 0, 1, function(arg) {
            /*global console*/
            console.log('cps _print: "' +arg +'" typeof', typeof arg
                                                    , 'object: ', arg);
            return arg;
        })
        /**
         * Constructor for a scaling transformation matrix
         */
      , new Operator('Scaling', false, 0, 0, 2, [
          ['number', 'number', function(x, y) {
              return transform.Scale(x, y);
          }]
        ])
      , new Operator('Translation', false, 0, 0, 2, [
            ['number', 'number', function(x, y) {
                return transform.Offset(x, y);
            }]
        ])
      , new Operator('Skew', false, 0, 0, 2, [
            ['number', 'number', function(x, y) {
                return transform.Identity.skew(x, y);
            }]
        ])
      , new Operator('Rotation', false, 0, 0, 1, [
            ['number', function(angle) {
                return transform.Identity.rotate(angle);
            }]
        ])
      , new Operator('Transformation', false, 0, 0, 6, [
            ['number', 'number', 'number', 'number', 'number', 'number'
            , function(xx, xy, yx, yy, dx, dy) {
                return new Transformation(xx, xy, yx, yy, dx, dy);
            }]
        ])
        /**
         * Return the identity transformation
         */
      , new Operator('Identity', false, 0, 0, 0, function(){
                                            return transform.Identity;})
      , new Operator('Selector', false, 0, 0, 1, [
            ['string'
            , function(selectorString) {
                return parseSelectorListFromString(selectorString);
            }]
        ])
    );

    engine.setBracketOperator('[', '__get__');
    engine.setNegateOperator('-', 'negate');
    return engine;
});
