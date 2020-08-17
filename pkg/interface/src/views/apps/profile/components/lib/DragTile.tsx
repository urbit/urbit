import React, { useMemo } from "react";
import { useDrag } from "react-dnd";
import { usePreview } from "react-dnd-multi-backend";
import { capitalize } from 'lodash';
import { TileTypeBasic, Tile } from "../../../../types/launch-update";

import { Box, Img, Text } from "@tlon/indigo-react";

interface DragTileProps {
  index: number;
  tile: Tile;
  title: string;
  style?: any;
}

function DragTileBox({ title, index, tile, ...props }: any) {
  const [, dragRef] = useDrag({
    item: { type: "launchTile", index, tile, title },
    collect: (monitor) => ({}),
  });

  return (
    <Box
      ref={dragRef}
      display="flex"
      alignItems="center"
      justifyContent="space-around"
      flexDirection="column"
      border={1}
      height="100%"
      width="100%"
      style={{ cursor: "move" }}
      {...props}
    ></Box>
  );
}

function DragTileCustom({ index, title, style }: any) {
  const tile = { type: { custom: null } };
  return (
    <DragTileBox bg="white" style={style} title={title} tile={tile} index={index}>
      <Text fontSize={1}>{capitalize(title)}</Text>
    </DragTileBox>
  );
}

function DragTileBasic(props: {
  tile: TileTypeBasic;
  index: number;
  style: any;
}) {
  const { basic: tile } = props.tile;
  const isDojo = useMemo(() => tile.title === "Dojo", [tile.title]);
  return (
    <DragTileBox
      tile={{ type: props.tile }}
      index={props.index}
      bg={isDojo ? "black" : "white"}
      style={props.style}
    >
      <Img width="48px" height="48px" src={tile.iconUrl} />
      <Text color={isDojo ? "white" : "black"}>{tile.title}</Text>
    </DragTileBox>
  );
}

export function DragTile(props: DragTileProps) {
  if ("basic" in props.tile.type) {
    return (
      <DragTileBasic
        index={props.index}
        style={props.style}
        tile={props.tile.type}
      />
    );
  } else {
    return (
      <DragTileCustom
        style={props.style}
        title={props.title}
        index={props.index}
      />
    );
  }
}

export function DragTilePreview() {
  let { display, style, item } = usePreview();

  if (!display) {
    return null;
  }

  style = { ...style, height: "96px", width: "96px", "z-index": "5" };
  return <DragTile style={style} {...item} />;
}
