import React, { useState, useRef, useEffect } from 'react';

import { Contact, Group } from '@urbit/api';
import { Box, ColProps } from '@tlon/indigo-react';

import ProfileOverlay, { OVERLAY_HEIGHT } from './ProfileOverlay';

type OverlaySigilProps = ColProps & {
  className: string;
  color: string;
  contact?: Contact;
  group?: Group;
  scrollWindow?: HTMLElement;
  ship: string;
  onDismiss: () => void;
};

interface OverlaySigilState {
  visible: boolean | false;
  space: {
    top: number | 'auto';
    bottom: number | 'auto';
  };
}

export const OverlaySigil = (props: OverlaySigilProps): React.FC => {
  const {
    className,
    color,
    contact,
    group,
    onDismiss,
    scrollWindow,
    ship,
    ...rest
  } = { ...props };
  const containerRef = useRef(null);
  const [visible, setVisible] = useState<OverlaySigilState['visible']>(false);
  const [space, setSpace] = useState<OverlaySigilState['space']>({
    top: 'auto',
    bottom: 'auto'
  });

  const updateContainerOffset = () => {
    if (scrollWindow && containerRef && containerRef.current) {
      const container = containerRef.current;
      const bottomSpace = scrollWindow
        ? scrollWindow.clientHeight -
          (container.getBoundingClientRect().top +
            OVERLAY_HEIGHT -
            scrollWindow.getBoundingClientRect().top)
        : 'auto';
      const topSpace = scrollWindow
        ? container.getBoundingClientRect().top -
          scrollWindow.getBoundingClientRect().top
        : 0;
      setSpace({
        ...space,
        top: topSpace,
        bottom: bottomSpace
      });
    }
    setVisible(true);
  };

  useEffect(() => {
    updateContainerOffset();
    if (scrollWindow)
      scrollWindow.addEventListener('scroll', updateContainerOffset);
    return () => {
      if (scrollWindow)
        scrollWindow.removeEventListener('scroll', updateContainerOffset, true);
    };
  }, [scrollWindow]);

  return (
    <Box
      cursor='pointer'
      position='relative'
      className={className}
      pr={0}
      pl={0}
      ref={containerRef}
      style={{ visibility: visible ? 'visible' : 'hidden' }}
    >
      <ProfileOverlay
        bottomSpace={space.bottom}
        color={color}
        contact={contact}
        group={group}
        onDismiss={onDismiss}
        ship={ship}
        topSpace={space.top}
        {...rest}
      />
    </Box>
  );
};

export default OverlaySigil;
