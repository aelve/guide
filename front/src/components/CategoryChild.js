import React, { Component } from 'react';
import * as T from '../types';
import { fetchData } from '../utils/index';
import { If } from 'jsx-control-statements';

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
    const uid = extractUid(this.props.match.params.uid);

    fetchData('http://localhost:8080/haskell/api/category/'+uid)
      .then(data => this.setState({cat: data}))
  }
    render() {

      const item = (uid, name) => {
        return ( <li key={uid}>{name}</li> )
      }

      return (
        <div>
          <h1>{ this.state.cat.title }</h1>
          <If condition={this.state.cat.description !== undefined}>
             { this.state.cat.description.html}
          </If>
          <If condition={this.state.cat.items !== undefined}>
              { this.state.cat.items.map(i => item(i.uid, i.name)) }
          </If>
        </div>
        )
    }
}

module.exports = { CategoryChild }