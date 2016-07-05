define([
    'Atem-Property-Language/errors'
  , './_Token'
], function(
    errors
  , Parent
) {
    "use strict";

    /**
     * Like whitespace or comments.
     */
    function _VoidToken(literal) {
        Parent.call(this, literal, 0, 0);
    }
    var _p = _VoidToken.prototype = Object.create(Parent.prototype);
    _p.constructor = _VoidToken;

    Object.defineProperty(_p, '_ejects', {
        value: 0
      , writable: false
    });

    return _VoidToken;
});
