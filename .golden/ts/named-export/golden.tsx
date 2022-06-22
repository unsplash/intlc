export const test: (x: { age: number; bold: (x: ReactElement) => ReactElement; currTime: Date; isDev: boolean; magicNumber: number; name: ('Sam' | 'Ashley') & string; todayDate: Date }) => ReactElement = x => <>Hello {x.bold(<>{x.name}</>)}! You are {(() => { switch (x.age as typeof x.age) { case 42: return <>very cool</>; default: { switch (new Intl.PluralRules('te-ST').select(x.age)) { case 'zero': return <>new around here</>; default: return <>not all that interesting</>; } } } })()}. Regardless, the magic number is most certainly {new Intl.NumberFormat('te-ST').format(x.magicNumber)}! The date is {new Intl.DateTimeFormat('te-ST', { dateStyle: 'short' }).format(x.todayDate)}, and the time is {new Intl.DateTimeFormat('te-ST', { timeStyle: 'full' }).format(x.currTime)}. And just to recap, your name is {(() => { switch (x.name as typeof x.name) { case 'Sam': return <>undoubtedly excellent</>; case 'Ashley': return <>fairly good</>; } })()}. Finally, you are {(() => { switch (x.isDev as typeof x.isDev) { case true: return <>a software engineer</>; case false: return <>something less fun</>; } })()}. Bonus: Some characters that might need escaping! ` ``</>