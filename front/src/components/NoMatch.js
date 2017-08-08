// @flow

import React, { Component } from 'react';
import { Link } from 'react-router-dom';

class NoMatch extends Component {
  render() {
    return (
      <div>
        <h1>Not Found</h1>
        <p><Link to="/haskell">Go home</Link></p>
      </div>
    )
  }
}

module.exports = {
    NoMatch
}
