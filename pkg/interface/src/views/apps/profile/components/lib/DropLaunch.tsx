import React, { useCallback, ReactNode } from "react";
import { useDrop } from "react-dnd";
import { DndProvider, usePreview } from "react-dnd-multi-backend";
import HTML5toTouch from "react-dnd-multi-backend/dist/esm/HTML5toTouch";
import { Box } from "@tlon/indigo-react";

import { DragTile, DragTilePreview } from "./DragTile";
import { useField } from "formik";

function DropLaunchTile({
  children,
  index,
  didDrop,
}: {
  index: number;
  children: ReactNode;
  didDrop: (item: number, location: number) => void;
}) {
  const onDrop = useCallback(
    (item: any, monitor: any) => {
      didDrop(item.index, index);
    },
    [index, didDrop]
  );

  const { display, style, item } = usePreview();

  const [{ isOver }, drop] = useDrop({
    accept: "launchTile",
    drop: onDrop,
    collect: (monitor) => ({
      isOver: !!monitor.isOver(),
    }),
  });

  return (
    <div
      ref={drop}
      style={{
        position: "relative",
        width: "100%",
        height: "100%",
      }}
    >
      {children}
    </div>
  );
}

export function DropLaunchTiles({ tiles, name }: any) {
  const [field, meta, helpers] = useField<string[]>(name);

  const { value } = meta;
  const { setValue } = helpers;

  const onChange = useCallback(
    (x: number, y: number) => {
      // swap tiles
      let t = value.slice();
      const c = t[x];
      t[x] = t[y];
      t[y] = c;
      setValue(t);
    },
    [setValue, value]
  );

  return (
    <DndProvider options={HTML5toTouch}>
      <Box
        display="grid"
        gridGap={2}
        gridTemplateColumns={["96px 96px", "96px 96px 96px 96px"]}
        gridAutoRows="96px"
      >
        <DragTilePreview />
        {value.map((tile, i) => (
          <DropLaunchTile didDrop={onChange} key={`${i}-${tile}`} index={i}>
            <DragTile title={tile} tile={tiles[tile]} index={i} />
          </DropLaunchTile>
        ))}
      </Box>
    </DndProvider>
  );
}
