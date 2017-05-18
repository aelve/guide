import React, { Component } from 'react';
import * as T from '../../types';
import ReactMarkdown from 'react-markdown';

const ItemDescription = (props : T.Item) => {
  return (
    <div className="item-description">
      <strong>Summary</strong>
      <ReactMarkdown className="notes-like" source={props.description.text} />
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
                  <ReactMarkdown className="section normal editable shown noscript-shown" 
                                  source={p.content.text}/>
                </li>) }
            </ul>
          </div>
          <div className="traits-group">
            <strong>Cons</strong>
            <ul>
              {props.cons.map(c => 
                <li key={c.uid}>
                  <ReactMarkdown className="section normal editable shown noscript-shown" 
                                  source={ c.content.text}/>
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
          <div className="notes-like" dangerouslySetInnerHTML={{__html: props.ecosystem.html}}/>
        </div>
      </div>
    </div>
  )
}

const NotesWrapper = (props : T.Item) => {
  return (
    <div className="notes-wrapper">
      <div className="item-notes">
        <a href="">
          <strong>Notes</strong>
        </a>
        <div className="shown collapsed section">
          <ReactMarkdown source={props.notes.text}/>
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