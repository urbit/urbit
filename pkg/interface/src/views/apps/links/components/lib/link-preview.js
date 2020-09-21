import React from 'react';
import { cite } from '~/logic/lib/util';
import moment from 'moment';

export const LinkPreview = (props) => {
  const showNickname = props.nickname && !props.hideNicknames;
  const nameClass = showNickname ? 'inter' : 'mono';

  const author = props.post.author;
  const timeSent = 
    moment.unix(props.post['time-sent'] / 1000).format('hh:mm a');

  const title = props.post.contents[0].text;
  const url = props.post.contents[1].url;


  return (
    <div className="pb6 w-100">
      <div className="flex flex-column ml2 pt6 flex-auto">
        <a href={url}
           className="w-100 flex"
           target="_blank"
           rel="noopener noreferrer">
          <p className="f8 truncate">{title}</p>
          <span className="gray2 ml2 f8 dib v-btm flex-shrink-0">
            {url} ↗
          </span>
        </a>
        <div className="w-100 pt1">
          <span className={'f9 pr2 white-d dib ' + nameClass} title={author}>
            {showNickname ? props.nickname : cite(`~${author}`)}
          </span>
          <span className="f9 inter gray2 pr3 dib">{timeSent}</span>
          <span className="f9 inter gray2 dib">
            {props.commentNumber} comments
          </span>
        </div>
      </div>
    </div>
  );
}

