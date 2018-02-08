define([
    'Atem-CPS-whitelisting/whitelisting'
], function(
    whitelisting
){
    "use strict";

    function UI(rule, property, astOperation, args) {
        this.rule = rule;
        this.property = property;
        this.astOperation = astOperation;

        this._arguments = args;
    }
    var _p = UI.prototype;

    _p._cps_getters = {
        arguments: 'arguments'
      , value: 'value'
    };
    _p.cpsGet = whitelisting.getMethod;
    _p.cpsHas = whitelisting.hasMethod;


    /**
     * This is a convenience property to access arguments[0]
     * it has no further special meaning.
     * The implementation of CPS UI gives the arguments their semantics.
     * I'd suggest the first argument is describing the value of the element.
     * This is on purpose vague! I want this as a marker and nothing more.
     */
    Object.defineProperty(_p, 'value', {
        get: function() { return this._arguments[0]; }
      , enumerable: true
    });

    Object.defineProperty(_p, 'arguments', {
        get: function() { return this._arguments; }
      , enumerable: true
    });

    return UI;
});
