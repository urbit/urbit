import React, { useCallback } from 'react';
import { Icon } from '@tlon/indigo-react';

export const InfoButton = () => {
  const onInfoClick = useCallback(() => {
    alert('To select text in the terminal, hold down the alt key.');
  }, []);

  return (
    <>
      <button className="info-btn" onClick={onInfoClick}>
        <Icon
          icon="Info"
          size="18px"
        />
      </button>
    </>
  );
};
