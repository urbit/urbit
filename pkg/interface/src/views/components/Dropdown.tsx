import React, {
  ReactNode,
  useState,
  useRef,
  useEffect,
  useCallback,
} from "react";
import styled from "styled-components";
import _ from "lodash";
import { Box, Col } from "@tlon/indigo-react";
import { useOutsideClick } from "~/logic/lib/useOutsideClick";
import { useLocation } from "react-router-dom";
import { Portal } from "./Portal";

type AlignY = "top" | "bottom";
type AlignX = "left" | "right";

interface DropdownProps {
  children: ReactNode;
  options: ReactNode;
  alignY: AlignY | AlignY[];
  alignX: AlignX | AlignX[];
  width?: string;
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

export function Dropdown(props: DropdownProps) {
  const { children, options } = props;
  const dropdownRef = useRef<HTMLElement>(null);
  const anchorRef = useRef<HTMLElement>(null);
  const { pathname } = useLocation();
  const [open, setOpen] = useState(false);
  const [coords, setCoords] = useState({});

  const updatePos = useCallback(() => {
    const rect = anchorRef.current?.getBoundingClientRect();
    if (rect) {
      const bounds = {
        top: rect.top,
        left: rect.left,
        bottom: document.documentElement.clientHeight - rect.bottom,
        right: document.documentElement.clientWidth - rect.right,
      };
      const alignX = _.isArray(props.alignX) ? props.alignX : [props.alignX];
      const alignY = _.isArray(props.alignY) ? props.alignY : [props.alignY];

      let newCoords = {
        ..._.reduce(
          alignX,
          (acc, a, idx) => ({
            ...acc,
            [a]: _.zipWith(
              [...Array(idx), `${bounds[a]}px`],
              acc[a] || [],
              (a, b) => a || b || null
            ),
          }),
          {}
        ),
        ..._.reduce(
          alignY,
          (acc, a, idx) => ({
            ...acc,
            [a]: _.zipWith(
              [...Array(idx), `${bounds[a]}px`],
              acc[a] || [],
              (a, b) => a || b || null
            ),
          }),
          {}
        ),
      };
      setCoords(newCoords);
    }
  }, [setCoords, anchorRef.current]);

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

  useEffect(() => {
    setOpen(false);
  }, [pathname]);

  useOutsideClick(dropdownRef, () => {
    setOpen(false);
  });

  return (
    <Box flexShrink={0} position={open ? "relative" : "static"}>
      <ClickBox ref={anchorRef} onClick={onOpen}>
        {children}
      </ClickBox>
      {open && (
        <Portal>
          <DropdownOptions
            width={props.width || "max-content"}
            {...coords}
            ref={dropdownRef}
          >
            {options}
          </DropdownOptions>
        </Portal>
      )}
    </Box>
  );
}

Dropdown.defaultProps = {
  alignX: "left",
  alignY: "bottom",
};
