// @flow

import React, { Component } from 'react';
import { GrandCategory } from './GrandCategory';
import { Tiles } from './Tiles';
import type { GrandCategoryT } from '../types';
import { fetchData } from '../utils/index';

class Home extends Component {
  state: {
    categories: Array<GrandCategoryT>;
  };
  componentWillMount() {
    this.setState({ categories: [] });
  }
  componentDidMount() {
    fetchData('http://localhost:8080/haskell/api/all-categories')
      .then(data => this.setState({categories: data}))
  }
    render() {
        return (
            <Tiles space="1em">
                {this.state.categories.map(grand => {
                    return <GrandCategory key={grand.title} val={grand} />;
                  })
                }
            </Tiles>
        );
    }
}

module.exports = { Home }
