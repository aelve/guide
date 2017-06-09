// @flow

import React, { Component } from 'react';
import * as T from '../types';
import { fetchData, extractUid, mkLink } from '../utils/index';
import { If, Choose, When } from 'jsx-control-statements';
import { Item } from './Item';


const CategoryStatusBanner = (props : T.Cat) => {
  const stub = "This category is a stub, contributions are welcome!";
  const wip  = "This category is a work in progress";
  function banner(text: string) {
    return (
      <div className="category-status-banner">
        <strong>{text}</strong>
        <style jsx>{`
          .category-status-banner {
            text-align: center;
            padding: 0.5em;
            margin: 0px 10%;
            background-color: #FFF694;
            border: 2px solid #202020;
          }
        `}</style>
      </div>
    )
  }
  return (
    <Choose>
      <When condition={ props.status === "Stub"     }>{ banner(stub) }</When>
      <When condition={ props.status === "WIP"      }>{ banner(wip)  }</When>
      <When condition={ props.status === "Finished" }><div/></When>
    </Choose>
  )
}

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
            <CategoryStatusBanner {...this.state.cat}/>
          </div>
          <div id={"category-notes-"+this.state.cat.uid} className="category-notes">
          <div className="noscript-shown shown normal section">
            <div className="notes-like">
              <div dangerouslySetInnerHTML={{__html: this.state.cat.description.html}}/>
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
            display: block;
            box-sizing: border-box;
            line-height: 19.2px;
            width: 762px;
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
