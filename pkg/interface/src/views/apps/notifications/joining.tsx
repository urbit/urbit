import React, { useState, useEffect } from "react";
import { Col, Text, SegmentedProgressBar } from "@tlon/indigo-react";
import GlobalApi from "~/logic/api/global";
import { JoinProgress, joinProgress, MetadataUpdatePreview, joinError } from "~/types";
import { clamp } from "~/logic/lib/util";

interface JoiningStatusProps {
  resource: string;
  api: GlobalApi;
  status: JoinProgress;
}

const description: string[] = 
  ["Attempting to contact group host",
    "Retrieving group data", 
    "Finished join", 
    "Unable to join group, you do not have the correct permissions",
    "Internal error, please file an issue"
  ];



export function JoiningStatus(props: JoiningStatusProps) {
  const { resource, status, api } = props;

  const [preview, setPreview] = useState<MetadataUpdatePreview | null>(null);

  useEffect(() => {
    (async () => {
      const prev = await api.metadata.preview(resource);
      setPreview(prev)
    })();
    return () => {
      setPreview(null);
    }
  }, [resource])

  const current = joinProgress.indexOf(status);
  const desc = description?.[current] || "";
  const title = preview?.metadata?.title ?? resource;
  const isError = joinError.indexOf(status as any) !== -1;
  return (
    <Col gapY="2">
      <Text fontSize="1">{isError ? "Failed to join " : "Joining "} {title}</Text>
      <Text color={isError ? "red" : "gray"}>{desc}</Text>
      <SegmentedProgressBar current={current + 1} segments={3} />
    </Col>
  );
}
