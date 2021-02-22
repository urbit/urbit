import React, { ReactNode, useMemo, useCallback } from "react";

import {
  FieldArray,
  FieldArrayRenderProps,
  Field,
  useFormikContext,
} from "formik";
import { Icon, Col, Row } from "@tlon/indigo-react";

interface ShuffleFieldsProps<N extends string> {
  name: N;
  children: (index: number, props: FieldArrayRenderProps) => ReactNode;
}

type Value<I extends string, T> = {
  [k in I]: T[];
};

export function ShuffleFields<N extends string, T, F extends Value<N, T>>(
  props: ShuffleFieldsProps<N>
) {
  const { name, children } = props;
  const { values } = useFormikContext<F>();
  const fields: T[] = useMemo(() => values[name], [values, name]);

  return (
    <FieldArray
      name={name}
      render={(arrayHelpers) => {
        const goUp = (i: number) => () => {
          if(i > 0) {
            arrayHelpers.swap(i - 1, i);
          }
        };
        const goDown = (i: number) => () => {
          if(i < fields.length - 1) {
            arrayHelpers.swap(i + 1, i);

          }
        };
        return (
          <Col gapY="3">
            {fields.map((field, i) => (
              <Row gapX="2" key={i}>
                {children(i, arrayHelpers)}
                <Icon icon="ChevronNorth" onClick={goUp(i)} />
                <Icon icon="ChevronSouth" onClick={goDown(i)} />
              </Row>
            ))}
          </Col>
        );
      }}
    />
  );
}
