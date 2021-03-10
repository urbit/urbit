import React, {
  ReactNode,
  useState,
  useRef,
  useEffect,
  useCallback,
  ReactElement
} from 'react';
import styled from 'styled-components';
import _ from 'lodash';
import { useLocation } from 'react-router-dom';

import { Box } from '@tlon/indigo-react';

import { useOutsideClick } from '~/logic/lib/useOutsideClick';
import { Portal } from './Portal';
import { getRelativePosition, AlignY, AlignX } from '~/logic/lib/relativePosition';

interface DropdownProps {
  children: ReactNode;
  options: ReactNode;
  alignY: AlignY | AlignY[];
  alignX: AlignX | AlignX[];
  offsetX?: number;
  offsetY?: number;
  width?: string;
  dropWidth?: string;
}

const ClickBox = styled(Box)`
  cursor: pointer;
`;

const DropdownOptions = styled(Box)`
  z-index: 20;
  position: fixed;
  transition: left 0.05s, top 0.05s, right 0.05s, bottom 0.05s;
  transition-timing-function: ease;
`;

export function Dropdown(props: DropdownProps): ReactElement {
  const { children, options, offsetX = 0, offsetY = 0 } = props;
  const dropdownRef = useRef<HTMLElement>(null);
  const anchorRef = useRef<HTMLElement>(null);
  const { pathname } = useLocation();
  const [open, setOpen] = useState(false);
  const [coords, setCoords] = useState({});

  const updatePos = useCallback(() => {
    const newCoords = getRelativePosition(anchorRef.current, props.alignX, props.alignY, offsetX, offsetY);
    if(newCoords) {
      setCoords(newCoords);
    }
  }, [setCoords, anchorRef.current, props.alignY, props.alignX]);

  useEffect(() => {
    if (!open) {
      return;
    }
    const interval = setInterval(updatePos, 100);
    return () => {
      clearInterval(interval);
    };
  }, [updatePos, open]);

  const onOpen = useCallback(
    (e: React.MouseEvent<HTMLDivElement>) => {
      updatePos();
      setOpen(true);
    },
    [setOpen, updatePos]
  );

  const close = useCallback(() => {
    setOpen(false);
  },[]);

  useEffect(() => {
    close();
  }, [pathname]);

  useOutsideClick(dropdownRef, close);

  const onOptionsClick = useCallback((e: any) => {
    e.stopPropagation();
  }, []);

  return (
    <Box flexShrink={props?.flexShrink ? props.flexShrink : 1} position={open ? 'relative' : 'static'} minWidth='0' width={props?.width ? props.width : 'auto'}>
      <ClickBox width='100%' ref={anchorRef} onClick={onOpen}>
        {children}
      </ClickBox>
      {open && (
        <Portal>
          <DropdownOptions
            width={props?.dropWidth || 'max-content'}
            {...coords}
            ref={dropdownRef}
            onClick={onOptionsClick}
          >
            {options}
          </DropdownOptions>
        </Portal>
      )}
    </Box>
  );
}

Dropdown.defaultProps = {
  alignX: 'left',
  alignY: 'bottom'
};
