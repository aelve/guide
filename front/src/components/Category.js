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
          <div className="category-info">
            <h2>
              <span className="controls">
                <a href="#" className="category-feed">
                  <img src="/rss_alt.svg" title="category-feed" alt="category feed"/>
                </a>
              </span>
              <a href={mkLink(this.state.cat)} className="category-title">{this.state.cat.title}</a>
              <span className="group">{this.state.cat.group}</span>
              <span className="text-button"><a href="#">edit</a></span>
              <span className="text-button"><a href="#">delete</a></span>
            </h2>
          </div>
          <div id={"category-notes-"+this.state.cat.uid} className="category-notes">
          <div className="noscript-shown shown normal section">
            <div className="notes-like">
              <ReactMarkdown source={this.state.cat.description.text}/>
            </div>
          </div>
          </div>
          <div>{ 
            this.state.cat.items
                .map(item => <Item key={item.uid} {...item}/>)
          }</div>
        </If>
        <style jsx>{`
          .Item {
            margin-top: 20px;
          }
          
          .category-info {
            display:flex;
            justify-content: flex-start;    
          }

          .category-info span {
            margin-left: 1em;
          }

          .text-button {
            font-size: 10pt;
            font-weight: normal;
          }

          .text-button::before {content: "[";}
          .text-button::after  {content: "]";}
 
          .text-button > a:visited {color: #008ACE;}
          h1           > a:visited {color: #008ACE;} 

          .category-info .group {
            font-size: 60%;
            font-weight: normal;
            color: gray;
          }

          .category {
            margin-top: 3em;
          }

          .category-title {
            font-weight: 600;
           }

           .category-info .controls {
             margin-right: 0.5em;
             vertical-align: -1px;
           } 

           .category-info .controls img {
             opacity: 0.3;
             height: 20px;
           }

          `}
        </style>
        </div>
      )
  }
}

module.exports = { Category }
