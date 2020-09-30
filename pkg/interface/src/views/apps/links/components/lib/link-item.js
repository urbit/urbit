import React from 'react';

import { Sigil } from '~/logic/lib/sigil';
import { Link } from 'react-router-dom';
import { cite } from '~/logic/lib/util';

export const LinkItem = (props) => {
  const {
    node,
    nickname,
    color,
    avatar,
    resource,
    hideAvatars,
    hideNicknames
  } = props;

  const URLparser = new RegExp(
    /((?:([\w\d\.-]+)\:\/\/?){1}(?:(www)\.?){0,1}(((?:[\w\d-]+\.)*)([\w\d-]+\.[\w\d]+))){1}(?:\:(\d+)){0,1}((\/(?:(?:[^\/\s\?]+\/)*))(?:([^\?\/\s#]+?(?:.[^\?\s]+){0,1}){0,1}(?:\?([^\s#]+)){0,1})){0,1}(?:#([^#\s]+)){0,1}/
  );

  const author = node.post.author;
  const index = node.post.index.split('/').join('-');
  const size = node.children ? node.children.size : 0;
  const contents = node.post.contents;
  const hostname = URLparser.exec(contents[1].url) ? URLparser.exec(contents[1].url)[4] : null;

  const showAvatar = avatar && !hideAvatars;
  const showNickname = nickname && !hideNicknames;

  const mono = showNickname ? 'inter white-d' : 'mono white-d';

  const img = showAvatar
    ? <img src={avatar} height={38} width={38} className="dib" />
    : <Sigil ship={`~${author}`} size={38} color={'#' + color} />;

  return (
    <div className='w-100 pv3 flex bg-white bg-gray0-d lh-solid'>
      {img}
      <div className='flex flex-column ml2 flex-auto'>
        <a
          href={contents[1].url}
          className='w-100 flex'
          target='_blank'
          rel='noopener noreferrer'>
          <p className='f8 truncate'>{contents[0].text}</p>
          <span className='gray2 ml2 f8 dib v-btm flex-shrink-0'>
            {hostname} ↗
          </span>
        </a>
        <div className='w-100'>
          <span className={'f9 pr2 white-d dib ' + mono} title={author}>
            {showNickname ? props.nickname : cite(author)}
          </span>
          <Link to={`/~link/${resource}/${index}`}>
            <span className='f9 inter gray2 dib'>{size} comments</span>
          </Link>
        </div>
      </div>
    </div>
  );
};

