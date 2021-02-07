/* eslint-disable max-lines-per-function */
import React, { useState, useRef, useEffect, PureComponent } from 'react';
import { Contact, Group } from '~/types';
import ProfileOverlay, { OVERLAY_HEIGHT } from './ProfileOverlay';
import { Box, ColProps } from '@tlon/indigo-react';

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
  clicked: boolean;
  topSpace: number | 'auto';
  bottomSpace: number | 'auto';
}

export const OverlayBox = (props) => {
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
  } = {
    ...props
  };
  const containerRef = useRef(null);

  const [space, setSpace] = useState({ top: 'auto', bottom: 'auto' });

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
        top: topSpace,
        bottom: bottomSpace
      });
    }
  };

  useEffect(() => {
    updateContainerOffset();
    scrollWindow.addEventListener('scroll', updateContainerOffset);
    return () => {
      scrollWindow.removeEventListener('scroll', updateContainerOffset, true);
    };
  }, []);

  return (
    <Box
      cursor='pointer'
      position='relative'
      className={className}
      pr={0}
      pl={0}
      ref={containerRef}
    >
      <ProfileOverlay
        api={api}
        bottomSpace={space.bottom}
        color={color}
        contact={contact}
        group={group}
        history={history}
        onDismiss={() => onDismiss()}
        ship={ship}
        topSpace={space.top}
        {...rest}
      />
    </Box>
  );
};
