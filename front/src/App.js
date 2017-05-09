// @flow

import React, { Component } from 'react';
import { Category, Home, NoMatch, Search, Header } from './components/index';
import { BrowserRouter as Router, Route, Switch } from 'react-router-dom';

class App extends Component {
  render() {
    return (
      <div className="App">
        <Header />
        <Search />
        <Router>
          <Switch>
            <Route exact path="/haskell" component={ Home } />
            <Route path="/haskell/:uid" component={ Category } />
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
