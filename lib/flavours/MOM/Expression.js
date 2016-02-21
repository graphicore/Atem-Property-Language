define([
    'Atem-Property-Language/errors'
  , 'Atem-Property-Language/_Expression'
  , './parser'
], function(
    errors
  , Parent
  , parser
) {
    "use strict";

    var ValueError = errors.Value
      , PropertyLanguageError = errors.PropertyLanguage
      ;

    function Expression(stack) {
        Parent.call(this);
        this._stack = stack;
    }

    var _p = Expression.prototype = Object.create(Parent.prototype);
    _p.constructor = Expression;

    Expression.factory = function(valueString) {
        var invalidMessage = null
          , stack = null
          ;
        try {
            stack = parser.parse(valueString);
        }
        catch(error) {
            if(!(error instanceof PropertyLanguageError))
                throw error;
            invalidMessage = error.message;
        }

        return [invalidMessage, stack && new Expression(stack)];
    };

    _p.evaluate = function(getAPI) {
        return this._stack.execute(getAPI);
    };

    _p.toString = function() {
        return '<' + this.constructor.name
             + ' with stack "' + this._stack + '">';
    };

    return Expression;
});
