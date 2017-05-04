import React, { Component } from 'react';
import * as T from '../types';
import { fetchData } from '../utils/index';

function extractUid(link : string) {
    const lastOccur = link.lastIndexOf('-');
    return link.slice(lastOccur + 1);
}

class CategoryChild extends Component {
  state: {
    cat: T.Cat;
  };
  componentWillMount() {
    this.setState({ cat: [] });
  }
  componentDidMount() {
    var uid = extractUid(this.props.match.params.uid);

    fetchData('http://localhost:8080/haskell/api/category/'+uid)
      .then(data => this.setState({cat: data}))
  }
    render() {
      return (
        <h1>{ this.state.cat.title }</h1>)
    }
}

module.exports = { CategoryChild }