// @flow

import React, { Component } from 'react';
import { GrandCategory, Tiles } from './components/index';
import { GrandCategoryT } from './types';

function checkStatus(response) {
  if (response.status >= 200 && response.status < 300) {
    return response;
  } else {
    throw new Error(response.statusText);
  }
}

function parseJSON(response) {
  return response.json();
}

class App extends Component {
  state: {
    categories: Array<GrandCategoryT>;
  };
  componentWillMount() {
    this.setState({ categories: [] });
  }
  componentDidMount() {
    fetch('http://localhost:8080/haskell/api/all-categories')
      .then(checkStatus).then(parseJSON)
      .then(data => {
        this.setState({ categories: data });
      }).catch(function(error) {
        console.log('request failed', error);
      });
  }
  render() {
    return (
      <div className="App">
        <Tiles space="1em">
          {this.state.categories.map(grand => {
             return <GrandCategory val={grand} />;
           })
          }
        </Tiles>
        <style jsx>{`
          .App {
            margin: auto;
            max-width: 800px;
          }
        `}</style>
      </div>
    );
  }
}

export default App;
