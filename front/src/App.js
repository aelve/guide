// @flow

import React, { Component } from 'react';
import renderIf from 'render-if';
import R from 'ramda';

function checkStatus(response) {
  if (response.status >= 200 && response.status < 300) {
    return response;
  } else {
    throw new Error(response.statusText);
  }
}

function parseJSON(response) {
  return response.json();
}

type Category = {
  uid   : string,
  link  : string,
  title : string
  };

type GrandCategoryT = {
  title    : string,
  finished : Array<Category>,
  wip      : Array<Category>,
  stubs    : Array<Category>
  }

class GrandCategory extends Component {
  props: {
    val: GrandCategoryT
  };
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

    const grand = this.props.val;

    return(
      <div className="GrandCategory" key={grand.title}>
        <h1>{grand.title}</h1>
        
        <div className="finished">
          {grand.finished.map(renderBigCatLink)}
        </div>

        {renderIf(!R.isEmpty(grand.wip))(
          <div className="wip">
            <h2>In progress</h2>
            <p>{R.intersperse(", ")(
                  grand.wip.map(renderSmallCatLink))}</p>
          </div>
        )}

        {renderIf(!R.isEmpty(grand.stubs))(
          <div className="stubs">
            <h2>To be written</h2>
            <p>{R.intersperse(", ")(
                  grand.stubs.map(renderSmallCatLink))}</p>
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
  state: {
    categories: Array<GrandCategoryT>;
  };
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
             return <GrandCategory val={grand} />;
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
