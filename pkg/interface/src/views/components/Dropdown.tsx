import React, { ReactNode, useState, useRef, useEffect } from "react";
import styled from "styled-components";
import { Box, Col } from "@tlon/indigo-react";
import { useOutsideClick } from "~/logic/lib/useOutsideClick";
import { useLocation } from "react-router-dom";

interface DropdownProps {
  children: ReactNode;
  options: ReactNode;
  position: "left" | "right";
  width?: string;
}

const ClickBox = styled(Box)`
  cursor: pointer;
`;

const DropdownOptions = styled(Box)<{ pos: string }>`
  z-index: 20;
  position: absolute;
  ${(p) => p.pos}: -1px;
`;

export function Dropdown(props: DropdownProps) {
  const { children, options } = props;
  const dropdownRef = useRef<HTMLElement>(null);
  const { pathname } = useLocation();

  useEffect(() => {
    setOpen(false);
  }, [pathname]);

  useOutsideClick(dropdownRef, () => {
    setOpen(false);
  });

  const [open, setOpen] = useState(false);

  const align = props.position === "right" ? "flex-end" : "flex-start";

  return (
    <Box position={open ? "relative" : "static"}>
      <ClickBox onClick={() => setOpen((o) => !o)}> {children}</ClickBox>
      {open && (
        <DropdownOptions pos={props.position} ref={dropdownRef}>
          <Col
            alignItems={align}
            width={props.width || "max-content"}
            border={1}
            borderColor="lightGray"
            bg="white"
            borderRadius={2}
          >
            {options}
          </Col>
        </DropdownOptions>
      )}
    </Box>
  );
}

Dropdown.defaultProps = {
  position: "left",
};
