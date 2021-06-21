import React, { MouseEvent, useCallback, useState } from 'react';
import { Row, Box, Icon, BaseAnchor } from '@tlon/indigo-react';

interface RemoteContentWrapperProps {
  children: JSX.Element;
  url: string;
  detail?: JSX.Element;
  flushPadding?: boolean;
  noOp?: boolean;
  tall?: boolean;
  embedOnly?: boolean;
}

export function RemoteContentWrapper(props: RemoteContentWrapperProps) {
  const {
    url,
    children,
    detail = null,
    flushPadding = false,
    noOp = false,
    tall = false,
    embedOnly = false
  } = props;

  const [unfold, setUnfold] = useState(false);
  const toggleUnfold = useCallback(() => {
    setUnfold(s => !s);
  }, []);

  const onClick = useCallback(
    (e: MouseEvent) => {
      noOp ? e.preventDefault() : e.stopPropagation();
    },
    [noOp]
  );

  const maxWidth = tall ? '100%' : 'min(500px, 100%)';

  if(embedOnly) {
    return detail || children;
  }

  return (
    <Box borderRadius={1} backgroundColor="washedGray" maxWidth={maxWidth}>
      <Row alignItems="center" gapX={1}>
        {!detail ? (
          <Icon ml={2} display="block" icon="ArrowExternal" />
        ) : (
          <Icon
            ml={2}
            display="block"
            onClick={toggleUnfold}
            icon={unfold ? 'TriangleSouth' : 'TriangleEast'}
          />
        )}
        <BaseAnchor
          display="flex"
          p={flushPadding ? 0 : 2}
          onClick={onClick}
          href={url}
          whiteSpace="nowrap"
          overflow="hidden"
          textOverflow="ellipsis"
          minWidth={0}
          width={!detail ? 'calc(100% - 24px)' : 'fit-content'}
          maxWidth={maxWidth}
          style={{ color: 'inherit', textDecoration: 'none' }}
          target="_blank"
          rel="noopener noreferrer"
          cursor={noOp ? 'default' : 'pointer'}
        >
          {children}
        </BaseAnchor>
      </Row>
      {detail}
    </Box>
  );
}
