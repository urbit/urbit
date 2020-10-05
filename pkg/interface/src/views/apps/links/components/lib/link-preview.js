import React from 'react';
import { cite } from '~/logic/lib/util';
import RemoteContent from "~/views/components/RemoteContent";

import moment from 'moment';


const URLparser = new RegExp(
    /((?:([\w\d\.-]+)\:\/\/?){1}(?:(www)\.?){0,1}(((?:[\w\d-]+\.)*)([\w\d-]+\.[\w\d]+))){1}(?:\:(\d+)){0,1}((\/(?:(?:[^\/\s\?]+\/)*))(?:([^\?\/\s#]+?(?:.[^\?\s]+){0,1}){0,1}(?:\?([^\s#]+)){0,1})){0,1}(?:#([^#\s]+)){0,1}/
);


export const LinkPreview = (props) => {
  const showNickname = props.nickname && !props.hideNicknames;
  const nameClass = showNickname ? 'inter' : 'mono';
  const author = props.post.author;
  const title = props.post.contents[0].text;
  const url = props.post.contents[1].url;
  const hostname = URLparser.exec(url) ? URLparser.exec(url)[4] : null;

  const timeSent =
    moment.unix(props.post['time-sent'] / 1000).format('hh:mm a');

  const embed = (
    <RemoteContent
      unfold={true}
      renderUrl={false}
      url={url}
      remoteContentPolicy={props.remoteContentPolicy}
      className="mw-100"
    />
  );

  return (
    <div className="pb6 w-100">
      <div className='w-100 tc'>{embed}</div>
      <div className="flex flex-column ml2 pt6 flex-auto">
        <a href={url}
           className="w-100 flex"
           target="_blank"
           rel="noopener noreferrer">
          <p className="f8 truncate">{title}</p>
          <span className="gray2 ml2 f8 dib v-btm flex-shrink-0" style={{ whiteSpace: 'nowrap' }}>
            {hostname} ↗
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

