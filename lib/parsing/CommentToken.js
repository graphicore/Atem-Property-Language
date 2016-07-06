define([
    'Atem-Property-Language/errors'
  , './_VoidToken'
], function(
    errors
  , Parent
) {
    "use strict";

    /**
     * Literal is a comment string.
     */
    function CommentToken(literal) {
        Parent.call(this, literal, 0, 0);
    }

    var _p = CommentToken.prototype = Object.create(Parent.prototype);
    _p.constructor = CommentToken;


    return CommentToken;
});
