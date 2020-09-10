import React from 'react';
import { CommentItem } from './comment-item';
import { getContactDetails } from '~/logic/lib/util';


export const Comments = (props) => {
  const { hideNicknames, hideAvatars } = props;
  console.log(props);

  /*const commentsList = Object.keys(commentsPage)
    .map((entry) => {
      const commentObj = commentsPage[entry];
      const { ship, time, udon } = commentObj;

      const contacts = props.contacts ? props.contacts : {};

      const { nickname, color, member, avatar } =
        getContactDetails(contacts[ship]);

      const nameClass = nickname && !hideNicknames ? 'inter' : 'mono';

      return(
        <CommentItem
          key={time}
          ship={ship}
          time={time}
          content={udon}
          nickname={nickname}
          hasNickname={Boolean(nickname)}
          color={color}
          avatar={avatar}
          member={member}
          hideNicknames={hideNicknames}
          hideAvatars={hideAvatars}
        />
      );
    });*/
  return (
    <div>
      { Array.from(props.comments.values()).map((comment) => {
          /*const commentObj = commentsPage[entry];
          const { ship, time, udon } = commentObj;

          const contacts = props.contacts ? props.contacts : {};

          const { nickname, color, member, avatar } =
            getContactDetails(contacts[ship]);

          const nameClass = nickname && !hideNicknames ? 'inter' : 'mono';*/
          console.log(comment);
          return (<div></div>);
        })
      }
    </div>
  );
}

