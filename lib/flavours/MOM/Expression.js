define([
    'Atem-Property-Language/errors'
  , 'Atem-Property-Language/parsing/PostfixExpression'
  , 'Atem-Property-Language/parsing/NameToken'
  , 'Atem-CPS/CPS/elements/SelectorList'
  , 'Atem-Property-Language/parsing/_Token'
  , './parser'
], function(
    errors
  , Parent
  , NameToken
  , SelectorList
  , _Token
  , parser
) {
    "use strict";

    var PropertyLanguageError = errors.PropertyLanguage;

    function Expression(ast) {
        this.ast = ast;
        var postfix = parser.astToPostfix(ast, true);
        Parent.call(this, postfix);
    }

    var _p = Expression.prototype = Object.create(Parent.prototype);
    _p.constructor = Expression;

    Expression.factory = function(valueString, selectorEngine) {
        var invalidMessage = null
          , ast = null
          , expression = null
          ;
        try {
            ast = parser.stringToAST(valueString, selectorEngine);
            expression = new Expression(ast);
        }
        catch(error) {
            if(!(error instanceof PropertyLanguageError))
                throw error;
            invalidMessage = error.message;
        }

        return [invalidMessage, expression];
    };

    _p.toString = function() {
        return parser.astToString(this.ast);
    };

    /**
     * This method is applied in PostfixExpression.evaluate, with the result
     * of the postfix stack evaluation.
     *
     * OperatorToken._convertTokenToValue does something similar.
     */
    _p._finalizeMethod = function(result, getAPI) {
        if(result instanceof NameToken)
            return getAPI.get(result.getValue());
        else if(result instanceof SelectorList) {
            var host = getAPI.get('this') // this can\'t be overidden by cps
              , node = getAPI.genericGetter(host, 'multivers')
              , item = getAPI.query(node, result)
              ;

            if(!item)
                throw new PropertyLanguageError('Not found: an element for '
                                                        + result);
            return item;
            // old, not fully subscribed:
            // return getAPI.get('this').multivers.query(result);
        }
        else if(result instanceof _Token)
            // maybe one day we allow stuff like operators as first class
            // values, but not now.
            throw new PropertyLanguageError('It is not allowed for an expression to '
                + 'resolve into a _Token, but this expression did: ' + result);
        return result;
    };

    return Expression;
});
