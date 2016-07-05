define([
    'Atem-Property-Language/errors'
  , './_VoidToken'
], function(
    errors
  , Parent
) {
    "use strict";

    /**
     * Literal is a string of whitespace.
     */
    function WhitespaceToken(literal) {
        Parent.call(this, literal, 0, 0);
    }

    var _p = WhitespaceToken.prototype = Object.create(Parent.prototype);
    _p.constructor = WhitespaceToken;


    return WhitespaceToken;
});
