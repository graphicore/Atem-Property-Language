define([
    'Atem-Property-Language/errors'
], function(
    errors
) {
    "use strict";

    var NotImplementedError = errors.NotImplemented;

    function _AbstractExpression() {}

    var _p = _AbstractExpression.prototype;
    _p.constructor = _AbstractExpression;

    // The concrete constructor is also expected to have a property "factory"
    // which is a function that it expected to return an array:
    // [invalidMessage (string), expressionInstance]
    // one of the two items must have a value, the other must be null
    // if the first item is set the construction failed
    // otherwise the second value must be a instance of the Expression
    // class with th interface described below.
    // Expression.factory = function(valueString)

    _p.evaluate = function(getAPI) {
        /*jshint unused:vars*/
        throw new NotImplementedError('The getValue Interface must be '
                                    + 'implemented by a subclass');
    };

    /**
     * This function is ought to return the code that is represented with
     * this expression. I.e. a string that can be parsed into an equivalent
     * expression.
     */
    _p.toString = function() {
        throw new NotImplementedError('The toString Interface must be '
                                    + 'implemented by a subclass');
    };

    return _AbstractExpression;
});
