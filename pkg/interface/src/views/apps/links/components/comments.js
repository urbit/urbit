import React from 'react';
import { CommentItem } from '~/views/components/CommentItem';


export const Comments = (props) => {
  const {
    hideNicknames,
    hideAvatars,
    remoteContentPolicy,
    ship,
    name
  } = props;

  const contacts = props.contacts ? props.contacts : {};

  return (
    <div>
      { Array.from(props.comments).reverse().map(([idx, comment]) => {
          return (
            <CommentItem
              comment={comment}
              key={idx}
              contacts={contacts}
              api={props.api}
              post={comment.post}
              ship={ship}
              name={name}
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

