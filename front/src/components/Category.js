// @flow

import React, { Component } from 'react';
import * as T from '../types';
import { fetchData, extractUid, mkLink } from '../utils/index';
import { If } from 'jsx-control-statements';
import { Item } from './Item';
import ReactMarkdown from 'react-markdown';

class Category extends Component {
  state: {
    cat : T.Cat
  };
  componentWillMount() {
    this.setState({ cat: undefined});
  }
  componentDidMount() {
    const uid = extractUid(this.props.match.params.uid);

    fetchData('http://localhost:8080/haskell/api/category/'+uid)
      .then((data : T.Cat) => this.setState({cat: data}))
  }
  render() {

    return (
      <div>
        <If condition={this.state.cat !== undefined}>
          <div className="cat-header">
          <a href={mkLink(this.state.cat)} className="category-title">{this.state.cat.title}</a>
          <span className="group">{this.state.cat.group}</span>
          <span className="text-button"><a href="#">edit</a></span>
          <span className="text-button"><a href="#">delete</a></span>
          </div>
          <div id={"category-notes-"+this.state.cat.uid} className="category-notes">
          <div className="noscript-shown shown normal section">
            <div className="notes-like">
              <ReactMarkdown source={this.state.cat.description.text}/>
            </div>
          </div>
          </div>
          <div>
              { this.state.cat.items
                  .map(item => <Item key={item.uid} {...item}/>)
              }
          </div>
        </If>
        <style jsx>{`
          .cat-header {
            display:flex;
            justify-content: flex-start;    
          }
          .cat-header span {
            margin-left: 1em;
          } 
          `}
        </style>
        </div>
      )
  }
}

module.exports = { Category }
