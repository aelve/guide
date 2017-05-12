// @flow

import React, { Component } from 'react';
import ReactMarkdown from 'react-markdown';
import * as T from '../types';
import { mkHackageUrl } from '../utils/index';
import { ItemInfo } from './Item/index';

class Item extends Component {
    render() {
      const item : T.Item = this.props;

        return (
          <div>
            <ItemInfo className="item-info" {...item} />
              <div className="item-body">
                <div className="item-description">
                  <strong>Summary</strong>
                  <ReactMarkdown className="notes-like" source={item.description.text} />
                </div>
                <div className="pros-cons-wrapper">
                  <div className="item-traits">
                    <div className="traits-groups-container">
                      <div className="traits-group">
                        <strong>Pros</strong>
                        <ul>
                          {item.pros.map(p => 
                            <li key={p.uid}>
                              <ReactMarkdown className="section normal editable shown noscript-shown" 
                                             source={p.content.text}/>
                            </li>) }
                        </ul>
                      </div>
                      <div className="traits-group">
                        <strong>Cons</strong>
                        <ul>
                          {item.cons.map(c => 
                            <li key={c.uid}>
                              <ReactMarkdown className="section normal editable shown noscript-shown" 
                                             source={ c.content.text}/>
                            </li>) }
                        </ul>
                      </div>
                    </div>
                  </div>
                </div>
                <div className="ecosystem-wrapper">
                  <div className="item-ecosystem">
                    <div className="noscript-shown show normal section">
                      <strong>Ecosystem</strong>
                      <div className="notes-like" dangerouslySetInnerHTML={{__html: item.ecosystem.html}}/>
                    </div>
                  </div>
                </div>
                <div className="notes-wrapper">
                  <div className="item-notes">
                    <a href="">
                      <strong>Notes</strong>
                    </a>
                    <div className="shown collapsed section">
                      <ReactMarkdown source={item.notes.text}/>
                    </div>
                    <div className="noscript-shown expanded section">
                    </div>
                  </div>
                </div>
              </div>
            <style>{`
              .item-info {
                background-color: #D6D6D6;
              }
              .item-info {
                padding-bottom: 12px;
                padding: 10px 15px;
              }

              .item-body {
                padding-top: 5px;
              }
              .item-traits,
              .item-notes,
              .item-description,
              .item-ecosystem {
                padding: 10px 15px 20px 15px;
              }   
              .item-traits {
                display:flex;
              }   
              
              .traits-groups-container {
                display: flex;
                flex-wrap: wrap;
                margin: 0px -15px;
              }

              .traits-group {
                flex: 1;
                min-width: 300px;
                padding: 0px 15px;
              }

              .traits-group > ul {
                padding-left: 1em;
              }

              .traits-group > ul li {
                margin: 10px 0px;
              }
            `}</style>
          </div>
        )
    }
}

module.exports = { Item }
