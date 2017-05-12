// @flow

import React, { Component } from 'react';
import * as T from '../../types';
import { mkHackageUrl } from '../../utils/index';

class ItemInfo extends Component {
    render() {
      const item : T.Item = this.props;
      
      return (
        <div className="item-info">
          <div className="section normal shown noscript-shown">
            <div>
              <a className="anchor" href={item.link}>#</a>
            </div>
            <div>
              <span className="item-name">{item.name + " "} 
                ( <a href={mkHackageUrl(item)}>Hackage</a> )
              </span>
            </div>
            <div className="item-group">
              {item.kind.tag}
            </div>
            <div className="controls">
              <span>
                <a href="#" className="move-item-up">
                  <img src="/arrow-thick-top.svg" alt="move item up" title="move item up"/>
                </a>
                <a>
                  <img src="/arrow-thick-bottom.svg" alt="move item down" title="move item down"/>
                </a>
              </span>
              <span>
                <a href="#" className="edit-item-info">
                  <img src="/cog.svg" alt="edit item info" title="edit item info"/>
                </a>
              </span>
              <span>
                <a href="#" className="delete-item">
                  <img src="/x.svg" alt="delete item" title="delete item"/>
                </a>
              </span>
            </div>
          </div>
          <style jsx>{`
            a.anchor {
              margin-right: 0.5em;
              color: gray;
            }

            .controls {
              display:flex;
              margin-left: auto;
            }

            .controls > span {
              white-space: nowrap;
            }

            /* on big screens we don't want to wrap the controls */
            @media (min-width: 480px) {
              .controls {
                white-space: nowrap;
              }
              .controls > span:first-child {
                padding-right: 1em;
              }
            }

            .controls img {
              opacity: 0.4;
              height: 20px;
              position: relative;
              bottom: -3px;
            }

            .section.normal {
              display: flex;
            }

            .item-info-edit-form label {
              display: block;
              margin-bottom: 5px;
              margin-top: 15px;
            }

            .item-info-edit-form {
              margin-top: 15px;
              margin-bottom: 5px;
            }

            .item-group {
              padding-left: 2em;
            }
          `}</style>
        </div>
      )
    }
}

module.exports = { ItemInfo }