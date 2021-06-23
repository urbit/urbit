import {
  Box,
  BoxProps,
  Col,
  Icon, Row
} from '@tlon/indigo-react';
import _ from 'lodash';
import React, { ReactNode, useCallback, useEffect, useMemo, useRef, useState } from 'react';
import VisibilitySensor from 'react-visibility-sensor';
import styled from 'styled-components';
import { getRelativePosition } from '~/logic/lib/relativePosition';
import { useOutsideClick } from '~/logic/lib/useOutsideClick';
import { useContact } from '~/logic/state/contact';
import { Portal } from './Portal';
import { ProfileStatus } from './ProfileStatus';
import RichText from './RichText';
import { Container } from './Container';
import { BoxLink, RowLink } from './Link';
import { ShipImage } from './ShipImage';
import { ShipName } from './ShipName';

export const OVERLAY_HEIGHT = 250;
const FixedOverlay = styled(Container)`
  position: fixed;
  -webkit-transition: all 0.1s ease-out;
  -moz-transition: all 0.1s ease-out;
  -o-transition: all 0.1s ease-out;
  transition: all 0.1s ease-out;
`;

type ProfileOverlayProps = BoxProps & {
  ship: string;
  children?: ReactNode;
  color?: string;
};

const ProfileOverlay = (props: ProfileOverlayProps) => {
  const {
    ship,
    children,
    ...rest
  } = props;

  const [open, _setOpen] = useState(false);
  const [coords, setCoords] = useState({});
  const [visible, setVisible] = useState(false);
  const outerRef = useRef<HTMLDivElement>(null);
  const innerRef = useRef<HTMLDivElement>(null);
  const isOwn = useMemo(() => window.ship === ship, [ship]);

  const contact = useContact(`~${ship}`);

  const setClosed = useCallback(() => {
    _setOpen(false);
  }, [_setOpen]);

  const setOpen = useCallback(() => {
    _setOpen(true);
  }, [_setOpen]);

  useEffect(() => {
    if(!visible) {
      setClosed();
    }
  }, [visible]);

  useOutsideClick(innerRef, setClosed);

  useEffect(() => {
    if(!open) {
      return () => {};
    }
    function _updateCoords() {
      if(outerRef.current) {
        const outer = outerRef.current;
        const { left, right, top } = outer.getBoundingClientRect();
        const spaceAtTop = top > 300;
        const spaceAtRight = right > 300 || right > left;
        setCoords(getRelativePosition(
          outer,
          spaceAtRight ? 'left' : 'right',
          spaceAtTop ? 'bottom' : 'top',
          -1* outer.clientWidth,
          -1 * outer.clientHeight
        ));
      }
    }
    const updateCoords = _.throttle(_updateCoords, 25);
    updateCoords();
    const interval = setInterval(updateCoords, 300);
    window.addEventListener('scroll', updateCoords);
    return () => {
      clearInterval(interval);
      window.removeEventListener('scroll', updateCoords);
    };
  }, [open]);

  return (
    <Box ref={outerRef} {...rest} onClick={setOpen} cursor="pointer">
      <VisibilitySensor active={open} onChange={setVisible}>
        {children}
      </VisibilitySensor>
  { open && (
    <Portal>
      <FixedOverlay
        ref={innerRef}
        {...coords}
        round
        zIndex={3}
        fontSize={0}
        height='250px'
        width='250px'
        padding={3}
        justifyContent='center'
      >
        {!isOwn && (
          <RowLink color='black' padding={3}
position='absolute' top={0}
left={0}
             to={`/~landscape/messages/dm/~${ship}`}
          >
             <Icon icon='Chat' size={16} />
         </RowLink>
         )}
        <BoxLink
          alignSelf='center'
          height='72px'
          to={`/~profile/~${ship}`}
          overflow='hidden'
          borderRadius={2}
        >
          <ShipImage ship={`~${ship}`} size={72} />
        </BoxLink>
        <Col
          position='absolute'
          overflow='hidden'
          minWidth={0}
          width='100%'
          padding={3}
          bottom={0}
          left={0}
        >
          <Row width='100%'>
            <ShipName copiable strong mb={0} ship={`~${ship}`} />
          </Row>
          {isOwn ? (
            <ProfileStatus
              ship={`~${ship}`}
              contact={contact}
            />
          ) : (
            <RichText
              display='inline-block'
              width='100%'
              minWidth={0}
              textOverflow='ellipsis'
              overflow='hidden'
              whiteSpace='pre'
              mb={0}
              disableRemoteContent
              gray
              title={contact?.status ? contact.status : ''}
            >
              {contact?.status ? contact.status : ''}
            </RichText>
          )}
        </Col>
      </FixedOverlay>
    </Portal>
    )}
  </Box>
  );
};

export default ProfileOverlay;
