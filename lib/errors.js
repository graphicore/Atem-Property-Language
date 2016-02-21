define([
  , 'Atem-Errors/errors'
], function(
    atemErrors
) {
    var errors = Object.create(atemErrors)
      , makeError = atemErrors.makeError.bind(null, errors)
      ;

    makeError('PropertyLanguage', undefined, errors.Error);
    makeError('PropertyLanguageParser', undefined, errors.PropertyLanguage);

    return errors;
});
