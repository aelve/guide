// @flow

function checkStatus(response : Response) {
  if (response.status >= 200 && response.status < 300) {
    return response;
  } else {
    throw new Error(response.statusText);
  }
}

function parseJSON(response : Response) {
  return response.json();
}

function fetchData(url : string) : Promise<any> {
  return fetch(url)
            .then(checkStatus)
            .then(parseJSON)
            .catch(function(error) {
                console.log('request failed', error);
            });
 }  

 module.exports = {
     fetchData
 }
