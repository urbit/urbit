import { Box, ColProps } from '@tlon/indigo-react';
import { Contact, Group } from '@urbit/api';
import React, { useEffect, useRef, useState } from 'react';
import ProfileOverlay, { OVERLAY_HEIGHT } from './ProfileOverlay';

type OverlaySigilProps = ColProps & {
  api: any;
  className: string;
  color: string;
  contact?: Contact;
  group?: Group;
  history: any;
  scrollWindow?: HTMLElement;
  ship: string;
};

interface OverlaySigilState {
  visible: boolean | false;
  space: {
    top: number | 'auto';
    bottom: number | 'auto';
  };
}

export const OverlaySigil = (props: OverlaySigilProps) => {
  const {
    api,
    className,
    color,
    contact,
    group,
    history,
    onDismiss,
    scrollWindow,
    ship,
    ...rest
  } = { ...props };
  const containerRef = useRef(null);
  const [visible, setVisible] = useState<OverlaySigilState.visible>();
  const [space, setSpace] = useState<OverlaySigilState.space>({
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
        api={api}
        bottomSpace={space.bottom}
        color={color}
        contact={contact}
        group={group}
        history={history}
        onDismiss={onDismiss}
        ship={ship}
        topSpace={space.top}
        {...rest}
      />
    </Box>
  );
};

export default OverlaySigil;
