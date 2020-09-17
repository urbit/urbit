import React, { Component } from 'react';
import urbitOb from 'urbit-ob';
import { Link } from 'react-router-dom';

import { InviteSearch } from '~/views/components/InviteSearch';
import { Spinner } from '~/views/components/Spinner';
import { makeRoutePath, deSig } from '~/logic/lib/util';

export const NewScreen = (props) => {
  const onClickCreate = () => {

    let name = 'a' + String(Math.floor(Math.random() * 100));
    let res = `/~tacryt-socryp/` + name;
    props.api.metadata.metadataAdd(
      'link',
      res,
      '/~tacryt-socryp/aaa',
      name,
      '',
      '~2000.1.1',
      '000000'
    );

    props.api.graph.addGraph(
      `~${window.ship}`,
      name,
      {},
      null
    );

  };

  return (
    <div>
      <button
        onClick={onClickCreate}>
        Start Chat
      </button>
    </div>
  );
};
