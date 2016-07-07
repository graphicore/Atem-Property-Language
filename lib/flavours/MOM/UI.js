define([
], function(
){
    "use strict";

    function UI(value/* , arguments */) {
        var i,l, args = [];
        for(i=1,l=arguments.length;i<l;i++)
            args.push(arguments[i]);
        this._value = value;
        this._arguments = args;
    }
    var _p = UI.prototype;

    // TODO: this should be whitelisted for CPS
    // then we can actually use the value in CPS
    Object.defineProperty(_p, 'value', {
        get: function(){
            return this._arguments[0]
                // offset argument
                ? this._value['+'](this._arguments[0])
                : this._value
                ;
        }
    });

    return UI;
});
