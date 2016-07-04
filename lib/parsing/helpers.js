define([], function(){
    "us strict";
    /**
     * check whether val is an integer
     *
     * Copy and paste from ufoJS/main
     */
    function isInt (n) {
        // n === n NaN will return false
        // n|0 rounds
        return typeof n === 'number' && n === n && n === (n|0);
    }

    /**
     * check whether val is a float
     *
     * Copy and paste from ufoJS/main
     */
    function isFloat (n) {
        // n === n NaN will return false
        // n|0 rounds
        return typeof n === 'number' && isFinite(n) && n !== (n|0);
    }

    /**
     * This is a wrapper around typeof and instanceof
     * it's there to make me type less and loosely inspired by the python
     * builtin instanceof.
     *
     * Copy and paste from ufoJS/main
     */
    function isInstance(
        value,
        types_ /* function or typeof string or a list of these */
    ) {
        if(arguments.length < 2)
            throw new TypeError(
                'isInstance() expects 2 arguments, got ' + arguments.length
            );
        var types = (types_ instanceof Array) ? types_ : [types_],
            typeOfType, i;
        for(i = 0; i < types.length; i++) {
            typeOfType = typeof types[i];
            if( typeOfType === 'function' && value instanceof types[i]
                || types[i] === 'int' && isInt(value)
                || types[i] === 'float' && isFloat(value)
                || types[i] === 'NaN' && value !== value
                || types[i] === 'null' && value === null
                || types[i] === 'Infinity' && value === Number.POSITIVE_INFINITY
                || types[i] === '-Infinity' && value === Number.NEGATIVE_INFINITY
                // this will test strings like 'number', 'undefined', string
                || typeOfType === 'string'
                    && typeof value === types[i]
                    && value === value /*not true for NaN*/
            )
                return true;
        }
        return false;
    }

    return {
        isInstance: isInstance
      , isFloat: isFloat
      , isInt: isInt
    };
});
