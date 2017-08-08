// @flow

import { fetchData } from './fetchData';
import * as T from '../types';

function extractUid(link : string) {
    const lastOccur = link.lastIndexOf('-');
    return link.slice(lastOccur + 1);
};

const mkLink = (cat : T.Cat) => cat.title.toLowerCase() + "-" + cat.uid;

const mkHackageUrl = (item : T.Item) => "https://hackage.haskell.org/package/" + item.kind.hackageName;

module.exports = {
    fetchData,
    extractUid,
    mkLink,
    mkHackageUrl
}
