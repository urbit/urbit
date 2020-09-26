import React from 'react';
import { CommentItem } from './comment-item';
import { getContactDetails } from '~/logic/lib/util';


export const Comments = (props) => {
  const {
    hideNicknames,
    hideAvatars,
    remoteContentPolicy
  } = props;

  const contacts = props.contacts ? props.contacts : {};

  return (
    <div>
      { Array.from(props.comments).map(([date, comment]) => {
          const { nickname, color, member, avatar } =
            getContactDetails(contacts[comment.post.author]);

          const nameClass = nickname && !hideNicknames ? 'inter' : 'mono';

          return (
            <CommentItem
              key={comment.post.index}
              post={comment.post}
              nickname={nickname}
              hasNickname={Boolean(nickname)}
              color={color}
              avatar={avatar}
              member={member}
              hideNicknames={hideNicknames}
              hideAvatars={hideAvatars}
              remoteContentPolicy={remoteContentPolicy}
            />
          );
        })
      }
    </div>
  );
}

