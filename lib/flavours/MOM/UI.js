define([
], function(
){
    "use strict";

    function UI(property, rule, args) {
        this.property = property;
        this.rule = rule;
        this._value = args[0];
        this._arguments = args.slice(1);
    }
    var _p = UI.prototype;

    // TODO: this should be whitelisted for CPS
    // then we can actually use the value in CPS
    Object.defineProperty(_p, 'value', {
        get: function() {
            return this._arguments[0]
                // offset argument
                ? this._value['+'](this._arguments[0])
                : this._value
                ;
        }
    });

    return UI;
});
