/* eslint-disable max-lines-per-function */
import React, { useState, useRef, useEffect, PureComponent } from 'react';

import { Sigil } from '~/logic/lib/sigil';
import { Contact, Group } from '~/types';

import ProfileOverlay, { OVERLAY_HEIGHT } from './ProfileOverlay';

import { Box, BaseImage, ColProps } from '@tlon/indigo-react';
import { withLocalState } from '~/logic/state/local';

type OverlaySigilProps = ColProps & {
  api: any;
  className: string;
  color: string;
  contact?: Contact;
  group?: Group;
  hideAvatars: boolean;
  history: any;
  scrollWindow?: HTMLElement;
  ship: string;
  sigilClass: string;
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
    hideAvatars,
    history,
    onDismiss,
    scrollWindow,
    ship,
    sigilClass,
    ...rest
  } = {
    ...props
  };
  const containerRef = useRef(null);

  const [space, setSpace] = useState({ top: 'auto', bottom: 'auto' });

  const updateContainerOffset = () => {
    if (containerRef && containerRef.current) {
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

class OverlaySigil extends PureComponent<OverlaySigilProps, OverlaySigilState> {
  public containerRef: React.Ref<HTMLDivElement>;

  constructor(props) {
    super(props);
    this.state = {
      clicked: false,
      topSpace: 0,
      bottomSpace: 0
    };

    this.containerRef = React.createRef();

    this.profileShow = this.profileShow.bind(this);
    this.profileHide = this.profileHide.bind(this);
    this.updateContainerOffset = this.updateContainerOffset.bind(this);
  }

  profileShow() {
    this.updateContainerOffset();
    this.setState({ clicked: true });
    this.props.scrollWindow?.addEventListener(
      'scroll',
      this.updateContainerOffset
    );
  }

  profileHide() {
    this.setState({ clicked: false });
    this.props.scrollWindow?.removeEventListener(
      'scroll',
      this.updateContainerOffset,
      true
    );
  }

  updateContainerOffset() {
    if (this.containerRef && this.containerRef.current) {
      const container = this.containerRef.current;
      const scrollWindow = this.props.scrollWindow;
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
      this.setState({
        topSpace,
        bottomSpace
      });
    }
  }

  componentWillUnmount() {
    this.props.scrollWindow?.removeEventListener(
      'scroll',
      this.updateContainerOffset,
      true
    );
  }

  render() {
    const {
      className,
      ship,
      contact,
      color,
      group,
      history,
      api,
      sigilClass,
      hideAvatars,
      pr = 0,
      pl = 0,
      ...rest
    } = this.props;

    const { state } = this;

    const img =
      contact && contact.avatar !== null && !hideAvatars ? (
        <BaseImage
          display='inline-block'
          src={contact.avatar}
          height={16}
          width={16}
        />
      ) : (
        <Sigil
          ship={ship}
          size={16}
          color={color}
          classes={sigilClass}
          icon
          padded
        />
      );

    return (
      <Box
        cursor='pointer'
        position='relative'
        onClick={this.profileShow}
        ref={this.containerRef}
        className={className}
        pr={pr}
        pl={pl}
      >
        {state.clicked && (
          <ProfileOverlay
            ship={ship}
            contact={contact}
            color={color}
            topSpace={state.topSpace}
            bottomSpace={state.bottomSpace}
            group={group}
            onDismiss={this.profileHide}
            history={history}
            api={api}
            {...rest}
          />
        )}
        {img}
      </Box>
    );
  }
}

export default withLocalState(OverlaySigil, ['hideAvatars']);
