define([
    'Atem-Property-Language/errors'
], function(
    errors
) {
    "use strict";

    var NotImplementedError = errors.NotImplemented;

    function _Expression() {}

    var _p = _Expression.prototype;
    _p.constructor = _Expression;

    // The concrete constructor is also expected to have a property "factory"
    // which is a function that it expected to return an array:
    // [invalidMessage (string), expressionInstance]
    // one of the two items must have a value, the other must be null
    // if the first item is set the construction failed
    // otherwise the second value must be a instance of the Expression
    // class with th interface described below.
    // Expression.factory = function(valueString)

    _p.evaluate = function(getAPI) {
        throw new NotImplementedError('The getValue Interface must be '
                                    + 'implemented by a subclass');
    };

    _p.getValue = function(getAPI) {
        errors.warn('The method "getValue" is deprecated! Use '
                                    +'"evaluate" instead,');
        return this.execute(getAPI);
    };

    _p.toString = function() {
        throw new NotImplementedError('The toString Interface must be '
                                    + 'implemented by a subclass');
    };

    return _Expression;
});
