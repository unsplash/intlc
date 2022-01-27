import React, { ReactElement } from 'react'
export const greeting: (x: { bold: (x: string) => string; name: string; age: number }) => string = x => `Hello ${x.bold(`${x.name}`)}, ${new Intl.NumberFormat('en-US').format(x.age)}!`
export const title: () => string = () => 'Unsplash'