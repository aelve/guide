import React, { Component } from 'react';

// Tiles grouped into columns
class Tiles extends Component {
  render() {
    return(
      <div className="Tiles">
        {this.props.children}

        <style jsx>{`
          .Tiles {
            display: flex;
            flex-flow: row wrap;
            justify-content: space-between;
            margin-left:  -{this.props.space};
            margin-right: -{this.props.space};
          }
          .Tiles > * {
            flex-grow: 1;
            flex-basis: 0;
            margin-left:  {this.props.space};
            margin-right: {this.props.space};
          }
        `}</style>
      </div>
    );
  }
}

module.exports = { Tiles }