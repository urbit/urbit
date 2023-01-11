import React, { useCallback } from 'react';
import { Icon } from '@tlon/indigo-react';
import { useDetectOS } from './lib/useDetectOS';

export const InfoButton = () => {
  const { isMacOS } = useDetectOS();

  const onInfoClick = useCallback(() => {
    const key = isMacOS ? 'alt' : 'shift';

    alert(`To select text in the terminal, hold down the ${key} key.`);
  }, [isMacOS]);

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
