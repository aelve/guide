import React, { Component } from 'react';
import ReactMarkdown from 'react-markdown';

class Item extends Component {
    render() {
        return (
          <div>
            <section>
              <h1>{this.props.name}</h1>
              <p>{this.props.kind.tag} {this.props.link}</p>
              <div dangerouslySetInnerHTML={{__html: this.props.ecosystem.html}}/>
              <ReactMarkdown source={this.props.notes.text}/>
            </section>
          </div>
        )
    }
}

module.exports = { Item }
