
import jQuery from 'jquery';

let $ = jQuery;

const config = {};
let token = null;

// Function ripped from Django docs.
// See: https://docs.djangoproject.com/en/dev/ref/csrf/#ajax
function csrfSafeMethod(method) {
    // These HTTP methods do not require CSRF protection.
    return (/^(GET|HEAD|OPTIONS|TRACE)$/i.test(method));
}

export function csrfPrefilter(options, ...args) {
    // The header should only be set when the request is local.
    if (!csrfSafeMethod(options.type) && !options.crossDomain) {
        const oldBeforeSend = options.beforeSend;
        options.beforeSend = function (xhr) {
            // The csrf token is valid for the duration of the session,
            // so it's safe to use a static token.
            xhr.setRequestHeader(config.key, token);
            if (oldBeforeSend) {
                oldBeforeSend(...args);
            }
        };
    }
}

export function setToken(newToken) {
    token = newToken;
}


/* Patch $.ajax to support expired CSRF tokens */
function addRetrySupport(retryURL, parseResponse, isCSRFFailure) {
    if (!isCSRFFailure) {
        isCSRFFailure = xhr => xhr.status === 403;
    }
    const originalAjax = $.ajax;

    /**
     * Copy properties from jqXhrToCopy to fakeJqXhr. This is makes fakeJqXhr
     * behave properly.
     */
    function fakeJqXhrInheritance(fakeJqXhr, jqXhrToCopy) {
        Object.keys(jqXhrToCopy).forEach((key) => {
            if (typeof jqXhrToCopy[key] === 'function') {
                fakeJqXhr[key] = jqXhrToCopy[key].bind(jqXhrToCopy);
            } else {
                fakeJqXhr[key] = jqXhrToCopy[key];
            }
        });
    }

    /**
     * Patch $.ajax to support expired csrf tokens. If a request is made and the
     * token is expired, then a new  token is fetched from the server. The original
     * request will be run again with the new token.
     *
     * For the outside world only 1 request is send, but depending on the situation
     * at most 3 request can be executed.
     */
    $.ajax = function (url, options) {
        const pResult = $.Deferred(); // eslint-disable-line new-cap
        const fakeJqXhr = pResult.promise();

        if (typeof url === 'object') {
            options = url;
            url = undefined;
        } else {
            options.url = url;
        }

        // The original ajax request might have success or error callbacks. We want
        // to trigger them manually based on if there is a csrf token mismatch.
        const success = options.success;
        const error = options.error;
        delete options.success;
        delete options.error;

        // Fire the first try!
        const xhrFirstTry = originalAjax(options);

        xhrFirstTry.error((jqXHR, textStatus, errorThrown) => {
            if (isCSRFFailure(jqXHR)) {
                // We assume that a csrf token mismatch happend, so fetch a new
                // token and retry with the correct token.
                originalAjax(retryURL).done((data) => {
                    setToken(parseResponse(data));
                    let xhrSecondTry = null;

                    options.success = (dataSecondSuccess, textStatusSecondSuccess, jqXHRSecondSuccess) => {
                        if (typeof success === 'function') success(dataSecondSuccess, textStatusSecondSuccess, jqXHRSecondSuccess);
                        pResult.resolve(dataSecondSuccess, textStatusSecondSuccess, jqXHRSecondSuccess);
                    };

                    options.error = (jqXHRSecondError, textStatusSecondError, errorThrownSecondError) => {
                        if (typeof error === 'function') error(jqXHRSecondError, textStatusSecondError, errorThrownSecondError);
                        pResult.reject(jqXHRSecondError, textStatusSecondError, errorThrownSecondError);
                    };

                    xhrSecondTry = originalAjax(options);
                    fakeJqXhrInheritance(fakeJqXhr, xhrSecondTry);
                });
            } else {
                // Some other error happend, so just pass it through.
                fakeJqXhrInheritance(fakeJqXhr, xhrFirstTry);
                if (typeof error === 'function') error(jqXHR, textStatus, errorThrown);
                pResult.reject(jqXHR, textStatus, errorThrown);
            }
        });

        // Upon success, update our fakeJqXhr and trigger the success callback.
        xhrFirstTry.success((data, textStatus, jqXHR) => {
            fakeJqXhrInheritance(fakeJqXhr, xhrFirstTry);
            if (typeof success === 'function') success(data, textStatus, jqXHR);

            pResult.resolve(data, textStatus, jqXHR);
        });

        fakeJqXhrInheritance(fakeJqXhr, xhrFirstTry);

        return fakeJqXhr;
    };
}


export function enable(newToken, newConfig) {
    newConfig || (newConfig = {});

    if (!newToken) {
        console.warn('CSRF token is not set!');
    }

    if (!newConfig.key) {
        newConfig.key = 'X-CSRF-TOKEN';
    }

    if (!newConfig.prefilter) {
        newConfig.prefilter = csrfPrefilter;
    }

    config.key = newConfig.key;

    if (newConfig.retry) {
        addRetrySupport(newConfig.retry.url, newConfig.retry.parseResponse,
            newConfig.retry.isCSRFFailure);
    }

    setToken(newToken);

    // Set a header on every request with the current csrf token in it.
    $.ajaxPrefilter(newConfig.prefilter);
}

export function mockJQuery(mockedJquery) {
    $ = mockedJquery;
}