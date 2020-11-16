import React, { PureComponent } from 'react';

import { Sigil } from '~/logic/lib/sigil';
import { Contact, Group } from '~/types';

import {
  ProfileOverlay,
  OVERLAY_HEIGHT
} from './ProfileOverlay';

import { Box, BaseImage, ColProps } from '@tlon/indigo-react';

type OverlaySigilProps = ColProps & {
  ship: string;
  contact?: Contact;
  color: string;
  sigilClass: string;
  group?: Group;
  hideAvatars: boolean;
  hideNicknames: boolean;
  scrollWindow?: HTMLElement;
  history: any;
  api: any;
  className: string;
}

interface OverlaySigilState {
  clicked: boolean;
  topSpace: number | 'auto';
  bottomSpace: number | 'auto';
}

export default class OverlaySigil extends PureComponent<OverlaySigilProps, OverlaySigilState> {
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
    this.props.scrollWindow?.addEventListener('scroll', this.updateContainerOffset);
  }

  profileHide() {
    this.setState({ clicked: false });
    this.props.scrollWindow?.removeEventListener('scroll', this.updateContainerOffset, true);
  }

  updateContainerOffset() {
    if (this.containerRef && this.containerRef.current) {
      const container = this.containerRef.current;
      const scrollWindow = this.props.scrollWindow;

      const bottomSpace = scrollWindow
        ? scrollWindow.scrollHeight - container.offsetTop - scrollWindow.scrollTop
        : 'auto';
      const topSpace = scrollWindow
        ? scrollWindow.offsetHeight - bottomSpace - OVERLAY_HEIGHT
        : 0;

      this.setState({
        topSpace,
        bottomSpace
      });
    }
  }

  componentWillUnmount() {
    this.props.scrollWindow?.removeEventListener('scroll', this.updateContainerOffset, true);
  }

  render() {
    const {
      className,
      ship,
      contact,
      color,
      group,
      hideAvatars,
      hideNicknames,
      history,
      api,
      sigilClass,
      ...rest
    } = this.props;

    const { state } = this;

    const img = (contact && (contact.avatar !== null) && !hideAvatars)
      ? <BaseImage display='inline-block' src={contact.avatar} height={16} width={16} />
      : <Sigil
        ship={ship}
        size={16}
        color={color}
        classes={sigilClass}
        icon
        padded
        />;

     return (
      <Box
        cursor='pointer'
        position='relative'
        onClick={this.profileShow}
         ref={this.containerRef}
         className={className}
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
            hideAvatars={hideAvatars}
            hideNicknames={hideNicknames}
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
