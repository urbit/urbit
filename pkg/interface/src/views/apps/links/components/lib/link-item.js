import React, { Component } from 'react';

import { Sigil } from '~/logic/lib/sigil';
import { Link } from 'react-router-dom';
import { cite } from '~/logic/lib/util';

export const LinkItem = (props) => {
  const {
    node,
    nickname,
    resource,
    hideAvatars,
    hideNicknames
  } = props;

  const author = node.post.author;
  const index = node.post.index.split('/').join('-');
  const size = node.children ? node.children.size : 0;
  const contents = node.post.contents;

  const showAvatar = props.avatar && !hideAvatars;
  const showNickname = nickname && !hideNicknames;

  const mono = showNickname ? 'inter white-d' : 'mono white-d';

  const img = showAvatar
    ? <img src={props.avatar} height={38} width={38} className="dib" />
    : <Sigil ship={`~${author}`} size={38} color={'#' + props.color} />;

  return (
    <div className="w-100 pv3 flex bg-white bg-gray0-d lh-solid">
      {img}
      <div className="flex flex-column ml2 flex-auto">
        <a href={contents[1].url}
           className="w-100 flex"
           target="_blank"
           rel="noopener noreferrer">
          <p className="f8 truncate">{props.title}</p>
          <span className="gray2 dib v-btm ml2 f8 flex-shrink-0">
            {contents[0].text} ↗
          </span>
        </a>
        <div className="w-100">
          <span className={'f9 pr2 pl2 dib ' + mono} title={author}>
            { showNickname ? nickname : cite(author) }
          </span>
          <Link to={`/~link/${resource}/${index}`}>
            <span className="f9 inter gray2 dib">{size} comments</span>
          </Link>
        </div>
      </div>
    </div>
  );
}

