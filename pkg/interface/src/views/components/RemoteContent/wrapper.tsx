import React, { MouseEvent, useCallback, useState } from 'react';
import { Row, Box, Icon, BaseAnchor } from '@tlon/indigo-react';
import AsyncFallback from '../AsyncFallback';

interface RemoteContentWrapperProps {
  children: JSX.Element;
  url: string;
  detail?: JSX.Element;
  flushPadding?: boolean;
  noOp?: boolean;
  tall?: boolean;
  embedOnly?: boolean;
  replaced?: boolean;
}

export function RemoteContentWrapper(props: RemoteContentWrapperProps) {
  const {
    url,
    children,
    detail = null,
    replaced = false,
    noOp = false,
    tall = false,
    embedOnly = false
  } = props;

  const [unfold, setUnfold] = useState(false);
  const toggleUnfold = useCallback((e: React.MouseEvent) => {
    e.stopPropagation();
    e.preventDefault();
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
    <Box width={detail ? '100%' : 'fit-content'} borderRadius={1} backgroundColor="washedGray" maxWidth={maxWidth}>
      <Row width="100%" alignItems="center" gapX={1}>
        {(!detail && !replaced) ? (
          <Icon ml={2} display="block" icon="ArrowExternal" />
        ) : !replaced ? (
          <Icon
            ml={2}
            display="block"
            onClick={toggleUnfold}
            icon={unfold ? 'TriangleSouth' : 'TriangleEast'}
          />
        ) : null }
        <BaseAnchor
          display="flex"
          p={replaced ? 0 : 2}
          onClick={onClick}
          href={url}
          whiteSpace="nowrap"
          overflow="hidden"
          textOverflow="ellipsis"
          minWidth={0}
          width={detail ? 'calc(100% - 24px)' : replaced ? '100%' : 'fit-content'}
          maxWidth={maxWidth}
          style={{ color: 'inherit', textDecoration: 'none' }}
          target="_blank"
          rel="noopener noreferrer"
          cursor={noOp ? 'default' : 'pointer'}
        >
          {children}
        </BaseAnchor>
      </Row>
      {unfold ? <AsyncFallback fallback={null} >{detail}</AsyncFallback> : null}
    </Box>
  );
}
