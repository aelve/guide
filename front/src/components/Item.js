// @flow

import React, { Component } from 'react';
import * as T from '../types';
import { ItemInfo, ItemBody } from './Item/index';

class Item extends Component {
    render() {
      const item : T.Item = this.props;

        return (
          <div>
            <ItemInfo className="item-info" {...item} />
            <ItemBody className="item-body" {...item} />
          </div>
        )
    }
}

module.exports = { Item }
