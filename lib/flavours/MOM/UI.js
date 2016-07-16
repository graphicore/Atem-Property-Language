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

        this._value = args[0];
        this._arguments = args;
    }
    var _p = UI.prototype;

    _p._cps_whitelist = {
        arguments: 'arguments'
      , value: 'value'
    };
    _p.cpsGet = whitelisting.getMethod;
    _p.cpsHas = whitelisting.hasMethod;


    Object.defineProperty(_p, 'value', {
        get: function() { return this._value; }
      , enumerable: true
    });

    Object.defineProperty(_p, 'arguments', {
        get: function() { return this._arguments; }
      , enumerable: true
    });

    return UI;
});
