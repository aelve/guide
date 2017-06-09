import React, { Component } from 'react';
import * as T from '../../types';

const ItemDescription = (props : T.Item) => {
  return (
    <div className="item-description">
      <strong>Summary</strong>
      <div className="notes-like"
           dangerouslySetInnerHTML={{__html: props.description.html}}/>
    </div>
  )
}

const ProsConsWrapper = (props : T.Item) => {
  return (
    <div className="pros-cons-wrapper">
      <div className="item-traits">
        <div className="traits-groups-container">
          <div className="traits-group">
            <strong>Pros</strong>
            <ul>
              {props.pros.map(p => 
                <li key={p.uid}>
                  <span className="section normal editable shown noscript-shown"
                        dangerouslySetInnerHTML={{__html: p.content.html}}/>
                </li>) }
            </ul>
          </div>
          <div className="traits-group">
            <strong>Cons</strong>
            <ul>
              {props.cons.map(c => 
                <li key={c.uid}>
                  <span className="section normal editable shown noscript-shown"
                        dangerouslySetInnerHTML={{__html: c.content.html}}/>
                </li>) }
            </ul>
          </div>
        </div>
      </div>
      <style jsx>{`
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

const EcosystemWrapper = (props : T.Item) => {
  return (
    <div className="ecosystem-wrapper">
      <div className="item-ecosystem">
        <div className="noscript-shown show normal section">
          <strong>Ecosystem</strong>
          <div className="notes-like"
               dangerouslySetInnerHTML={{__html: props.ecosystem.html}}/>
        </div>
      </div>
    </div>
  )
}

const NotesWrapper = (props : T.Item) => {
  return (
    <div className="notes-wrapper">
      <div className="item-notes notes-like">
        <a href="">
          <strong>Notes</strong>
        </a>
        <div className="shown collapsed section">
          <div dangerouslySetInnerHTML={{__html: props.notes.html}}/>
        </div>
        <div className="noscript-shown expanded section">
        </div>
      </div>
    </div>
  )
}

class ItemBody extends Component {   
    render() {
      const item : T.Item = this.props;

      return (
        <div className="item-body">
          <div>{ItemDescription(item)}</div>
          <div>{ProsConsWrapper(item)}</div>
          <div>{EcosystemWrapper(item)}</div>
          <div>{NotesWrapper(item)}</div>
          <style jsx>{`
            .item-body {
              padding-top: 15px;
              background-color: #F0F0F0;
              padding-left: 15px;
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
          `}</style>
        </div>
      )
    }
}

module.exports = { ItemBody }
