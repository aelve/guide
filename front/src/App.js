// @flow

import React, { Component } from 'react';
import { CategoryChild, Home, NoMatch } from './components/index';
import { BrowserRouter as Router, Route, Switch } from 'react-router-dom';

class App extends Component {
  render() {
    return (
      <div className="App">
        <Router>
          <Switch>
            <Route exact path="/" component={ Home } />
            <Route path="/haskell/:uid" component={ CategoryChild } />
            <Route component={ NoMatch } />
          </Switch>
        </Router>
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
