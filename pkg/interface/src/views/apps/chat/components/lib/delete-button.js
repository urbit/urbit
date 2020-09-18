import React, { memo } from 'react';

export const DeleteButton = memo(({ isOwner, station, changeLoading, association, contacts, api }) => {
  const leaveButtonClasses = (!isOwner) ? 'pointer' : 'c-default';
  const deleteButtonClasses = (isOwner) ? 
    'b--red2 red2 pointer bg-gray0-d' :
    'b--gray3 gray3 bg-gray0-d c-default';

  const deleteChat = () => {
    changeLoading(
      true,
      true,
      isOwner ? 'Deleting chat...' : 'Leaving chat...',
      () => {
        api.chat.delete(station);
      }
    );
  };

  const groupPath = association['group-path'];
  const unmanagedVillage = !contacts[groupPath];

  return (
    <div className="w-100 cf">
      <div className={'w-100 fl mt3 ' + ((isOwner) ? 'o-30' : '')}>
        <p className="f8 mt3 lh-copy db">Leave Chat</p>
        <p className="f9 gray2 db mb4">
          Remove this chat from your chat list.{' '}
          {unmanagedVillage 
            ? 'You will need to request for access again'
            : 'You will need to join again from the group page.'
          }
        </p>
        <a onClick={(!isOwner) ? deleteChat : null}
           className={
             'dib f9 black gray4-d bg-gray0-d ba pa2 b--black b--gray1-d ' +
             leaveButtonClasses
           }>
          Leave this chat
        </a>
      </div>
      <div className={'w-100 fl mt3 ' + ((!isOwner) ? 'o-30' : '')}>
        <p className="f8 mt3 lh-copy db">Delete Chat</p>
          <p className="f9 gray2 db mb4">
            Permanently delete this chat.{' '}
            All current members will no longer see this chat.
          </p>
          <a onClick={(isOwner) ? deleteChat : null}
           className={'dib f9 ba pa2 ' + deleteButtonClasses}
          >Delete this chat</a>
      </div>
    </div>
  );
})