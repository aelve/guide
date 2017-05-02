import React, { Component } from 'react';
import { If } from 'jsx-control-statements';
import R from 'ramda';
import { GrandCategoryT } from '../types';

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

        <If condition={!R.isEmpty(grand.wip)}>
          <div className="wip">
            <h2>In progress</h2>
            <p>{R.intersperse(", ")(grand.wip.map(renderSmallCatLink))}</p>
          </div>
        </If>

        <If condition={!R.isEmpty(grand.stubs)}>
          <div className="stubs">
            <h2>To be written</h2>
            <p>{R.intersperse(", ")(grand.stubs.map(renderSmallCatLink))}</p>
          </div>
        </If>

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

module.exports = { GrandCategory }
