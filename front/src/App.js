import React, { Component } from 'react';
import renderIf from 'render-if';
import R from 'ramda';

function checkStatus(response) {
  if (response.status >= 200 && response.status < 300) {
    return response;
  } else {
    var error = new Error(response.statusText);
    error.response = response;
    throw error;
  }
}

function parseJSON(response) {
  return response.json();
}

class GrandCategory extends Component {
  render() {
    const renderBigCatLink = cat => {
      return(
        <span>
          <a key={cat.uid} href={cat.link}>{cat.title}</a>
          <style jsx>{`
            a {
              display: block;
              font-size: 21px;
              line-height: 28px;
            }
          `}</style>
        </span>
      );
    };

    const renderSmallCatLink = cat => {
      return(
        <span>
          <a key={cat.uid} href={cat.link}>{cat.title}</a>
          <style jsx>{`
            a {white-space: nowrap;}
          `}</style>
        </span>
      );
    };

    return(
      <div className="GrandCategory" key={this.props.title}>
        <h1>{this.props.title}</h1>
        
        <div className="finished">
          {this.props.finished.map(renderBigCatLink)}
        </div>

        {renderIf(!R.isEmpty(this.props.wip))(
          <div className="wip">
            <h2>In progress</h2>
            <p>{R.intersperse(", ")(
                  this.props.wip.map(renderSmallCatLink))}</p>
          </div>
        )}

        {renderIf(!R.isEmpty(this.props.stubs))(
          <div className="stubs">
            <h2>To be written</h2>
            <p>{R.intersperse(", ")(
                  this.props.stubs.map(renderSmallCatLink))}</p>
          </div>
        )}

        <style jsx>{`
          .GrandCategory :global(*) {
            font-weight: 600;
          }
          .GrandCategory {
            width: 330px;
            margin-bottom: 30px;
          }
          .GrandCategory > div {
            margin-left: 2em;
          }
          .wip h2, .stubs h2 {
            font-size: 15px;
            margin-top: 1em;
            margin-bottom: 0px;
          }
          .wip p, .stubs p {
            padding-left: 2em;
            margin: 1px 0px;
            line-height: 18px;
            font-size: 15px;
          }
        `}</style>
      </div>
    );
  }
}

class App extends Component {
  componentWillMount() {
    this.setState({ categories: [] });
  }
  componentDidMount() {
    fetch('http://localhost:8080/haskell/api/all-categories')
      .then(checkStatus).then(parseJSON)
      .then(data => {
        this.setState({ categories: data });
      }).catch(function(error) {
        console.log('request failed', error);
      });
  }
  render() {
    return (
      <div className="App">
        <Tiles space="1em">
          {this.state.categories.map(grand => {
             return <GrandCategory {...grand} />;
           })
          }
        </Tiles>
        <style jsx>{`
          .App {
            margin: auto;
            max-width: 800px;
          }
        `}</style>
      </div>
    );
  }
}

export default App;

//
// Utils
//

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
